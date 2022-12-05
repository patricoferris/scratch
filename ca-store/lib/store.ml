module type S = Store_intf.S

module Version = Version
module Serialiser = Serialiser
module Contents = Contents

module Mem
    (S : Serialiser.S)
    (H : Hash.S)
    (C : Contents.S with type serial = S.t) =
struct
  type hash = H.t
  type content = C.t
  type serial = C.serial
  type t = (H.t, serial) Hashtbl.t

  module Cs = Contents.Make (S) (C)
  module Hd = Hdiff.Make (H) (S)
  module Vs = Version.Make (S)

  let empty : unit -> t = fun () -> Hashtbl.create 128

  let backward (tbl : t) sdiff =
    let rec loop acc diff hd =
      match Hd.prev hd with
      | None -> (S.empty, acc)
      | Some prev ->
          let diff = Hd.deserialise diff in
          loop (diff :: acc) (Hashtbl.find tbl prev) diff
    in
    let diff = Hd.deserialise sdiff in
    loop [ diff ] sdiff diff

  let fast_forward root diffs =
    List.fold_left
      (fun (v, s) d ->
        match Hd.diff d with
        | Some diff -> (Some (Hd.version d), S.Diff.apply s diff)
        | None -> (v, s))
      (None, root) diffs

  let find_raw tbl h =
    match Hashtbl.find_opt tbl h with
    | None -> None
    | Some c ->
        let root, diffs = backward tbl c in
        let version, current = fast_forward root diffs in
        Some (Option.get version, current)

  let find tbl h =
    Option.map
      (fun (k, v) ->
        if Version.compare C.version k = 0 then C.deserialise v
        else failwith "Version mismatch")
      (find_raw tbl h)

  let rec drop_last acc = function
    | [] | [ _ ] -> List.rev acc
    | x :: xs -> drop_last (x :: acc) xs

  let history (tbl : t) h =
    let rec forward acc (version : Version.t) ser diff hash =
      let next_ser = Option.map (S.Diff.apply ser) (Hd.diff diff) in
      match (next_ser, Hashtbl.find_opt tbl hash) with
      | Some next_ser, Some diff ->
          let next_hash = S.serialise ser |> H.digest in
          let _, c = Cs.unwrap next_ser in
          let hd = Hd.deserialise diff in
          let v =
            let hd = Hd.deserialise (Hashtbl.find tbl next_hash) in
            Hd.version hd
          in
          let acc = (v, c, next_hash) :: acc in
          forward acc version next_ser hd next_hash
      | None, Some diff ->
          (* Slight weirdness in how we store version numbers at the end of the chain *)
          let rest = if List.length acc < 1 then [] else List.tl acc in
          let _, s, h =
            if List.length acc = 0 then (version, ser, hash) else List.hd acc
          in
          let v = Hd.version (Hd.deserialise diff) in
          List.rev ((v, s, h) :: rest)
      | _ -> List.rev acc
    in
    match Hashtbl.find_opt tbl h with
    | None -> []
    | Some t -> (
        let root, diffs = backward tbl t in
        let (version, current, diff), acc =
          List.fold_left
            (fun (((_, s, _) as t), acc) d ->
              match Hd.diff d with
              | None -> (t, acc)
              | Some diff ->
                  let apply = S.Diff.apply s diff in
                  let next_hash = H.digest (S.serialise apply) in
                  let ((v, s, _) as t) =
                    ( Some (Hd.version d),
                      apply,
                      Option.map Hd.deserialise
                      @@ Hashtbl.find_opt tbl next_hash )
                  in
                  (t, (v, s, next_hash) :: acc))
            ((None, root, None), [])
            diffs
        in
        let root_to_current =
          List.filter_map
            (function Some v, s, h -> Some (v, s, h) | _ -> None)
            (List.rev acc)
        in
        match diff with
        | Some diff ->
            drop_last [] root_to_current
            @ forward [] (Option.get version) current diff
                (S.serialise current |> H.digest)
        | None -> root_to_current)

  let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs
  let latest t h = history t h |> last

  let update tbl hash v =
    Hashtbl.remove tbl hash;
    Hashtbl.add tbl hash v

  let add ?prev tbl c =
    match prev with
    | None ->
        let new_c = C.serialise c in
        let diff = S.Diff.diff S.empty new_c in
        let diff_hash = H.digest @@ S.serialise @@ S.Diff.serialise diff in
        let s = S.serialise S.empty in
        let prev = H.digest s in
        let hashed_diff = Hd.v ~verified:prev ~diff C.version |> Hd.serialise in
        let entry =
          Hd.v ~prev:diff_hash ~verified:prev C.version |> Hd.serialise
        in
        let hash = S.serialise new_c |> H.digest in
        Hashtbl.add tbl diff_hash hashed_diff;
        Hashtbl.add tbl hash entry;
        hash
    | Some hash ->
        let stored = Hashtbl.find tbl hash in
        let root, hashed_diffs = backward tbl stored in
        let _prev_version, prev = fast_forward root hashed_diffs in
        let prev_serialised = S.normalise prev in
        let prev_hash = prev_serialised |> S.serialise |> H.digest in
        let prev_hd = Hd.deserialise stored in
        let content = C.serialise c in
        let latest_diff = S.Diff.diff prev_serialised content in
        update tbl prev_hash
          (Hd.serialise
          @@ Hd.v ?prev:(Hd.prev prev_hd) ~diff:latest_diff
               ~verified:(Hd.verified prev_hd) (Hd.version prev_hd));
        let v = Hd.v ~prev:prev_hash ~verified:prev_hash C.version in
        let t = Hd.serialise v in
        let new_hash = H.digest (S.serialise content) in
        Hashtbl.add tbl new_hash t;
        new_hash

  let dump t =
    Hashtbl.iter
      (fun k s -> Fmt.pr "%a: %s@." (H.pp ~short:true) k (S.serialise s))
      t
end

module SHA256 = struct
  type t = string

  let digest s = Digestif.SHA256.(digest_string s |> to_raw_string)
  let to_hex v = Digestif.SHA256.(to_hex @@ of_raw_string v)
  let of_hex v = Digestif.SHA256.(to_raw_string @@ consistent_of_hex v)

  let pp ?(short = false) ppf t =
    if short then Format.pp_print_string ppf (String.sub (to_hex t) 0 7)
    else Format.pp_print_string ppf (to_hex t)
end
