module type S = Store_intf.S

module Version = Version
module Serialiser = Serialiser
module Contents = Contents

type ('a, 'b) mem = { prev : ('a, 'b) Hashtbl.t; diffs : ('a, 'b) Hashtbl.t }

module Mem
    (S : Serialiser.S)
    (H : Hash.S)
    (C : Contents.S with type serial = S.t) =
struct
  type hash = H.t
  type hashes = { content : hash; verifiable : hash }
  type content = C.t
  type serial = C.serial
  type t = (H.t, serial) mem

  module Cs = Contents.Make (S) (C)
  module Hd = Hdiff.Make (H) (S)
  module Vs = Version.Make (S)

  let empty : unit -> t =
   fun () -> { prev = Hashtbl.create 128; diffs = Hashtbl.create 128 }

  let backward (tbl : t) ~prev ~diff =
    let rec loop acc diff prev =
      match
        (diff, Option.map (fun prev -> H.of_hex (S.to_string prev)) prev)
      with
      | None, None -> (S.empty, acc)
      | Some _, None -> assert false
      | None, Some prev ->
          loop acc
            (Hashtbl.find_opt tbl.diffs prev)
            (Hashtbl.find_opt tbl.prev prev)
      | Some diff, Some prev ->
          let sdiff = Hd.deserialise diff in
          loop (sdiff :: acc)
            (Hashtbl.find_opt tbl.diffs prev)
            (Hashtbl.find_opt tbl.prev prev)
    in
    loop [] diff (Some prev)

  let fast_forward root diffs =
    List.fold_left
      (fun (v, s) d ->
        match Hd.diff d with
        | Some diff -> (Some (Hd.version d), S.Diff.apply s diff)
        | None -> (v, s))
      (None, root) diffs

  let find_raw (tbl : t) h =
    match Hashtbl.find_opt tbl.prev h with
    | None -> None
    | Some prev ->
        let diff = Hashtbl.find_opt tbl.diffs (S.to_string prev |> H.of_hex) in
        let root, diffs = backward tbl ~prev ~diff in
        let version, current = fast_forward root diffs in
        Some (Option.get version, current)

  let find tbl h =
    Option.map
      (fun (k, v) ->
        if Version.compare C.version k = 0 then C.deserialise v
        else failwith "Version mismatch")
      (find_raw tbl h)

  let rec _drop_last acc = function
    | [] | [ _ ] -> List.rev acc
    | x :: xs -> _drop_last (x :: acc) xs

  let history (tbl : t) (h : hash) =
    let rec forward acc (version : Version.t) ser diff hash =
      let next_ser = Option.map (S.Diff.apply ser) (Hd.diff diff) in
      match (next_ser, Hashtbl.find_opt tbl.diffs hash) with
      | Some next_ser, Some diff ->
          let next_hash = S.serialise ser |> H.digest in
          let _, c = Cs.unwrap next_ser in
          let hd = Hd.deserialise diff in
          let v, verifiable =
            let hd = Hd.deserialise (Hashtbl.find tbl.diffs next_hash) in
            (Hd.version hd, Hd.verified hd)
          in
          let acc = (v, c, { content = next_hash; verifiable }) :: acc in
          forward acc version next_ser hd next_hash
      | None, Some diff ->
          let verifiable =
            Hashtbl.find tbl.diffs h |> Hd.deserialise |> Hd.verified
          in
          (* Slight weirdness in how we store version numbers at the end of the chain *)
          let rest = if List.length acc < 1 then [] else List.tl acc in
          let _, s, h =
            if List.length acc = 0 then
              (version, ser, { content = h; verifiable })
            else List.hd acc
          in
          let v, verify =
            let hd = Hd.deserialise diff in
            (Hd.version hd, Hd.verified hd)
          in
          List.rev ((v, s, { h with verifiable = verify }) :: rest)
      | _ -> List.rev acc
    in
    match Hashtbl.find_opt tbl.prev h with
    | None -> []
    | Some prev -> (
        let diff = Hashtbl.find_opt tbl.diffs h in
        let root, diffs = backward tbl ~prev ~diff in
        let (version, current, diff), acc =
          List.fold_left
            (fun (((_, s, _) as t), acc) d ->
              match Hd.diff d with
              | None -> (t, acc)
              | Some diff ->
                  let verifiable = Hd.verified d in
                  let apply = S.Diff.apply s diff in
                  let next_hash = H.digest (S.serialise apply) in
                  let ((v, s, _) as t) =
                    ( Some (Hd.version d),
                      apply,
                      Option.map Hd.deserialise
                      @@ Hashtbl.find_opt tbl.diffs next_hash )
                  in
                  (t, (v, s, { content = next_hash; verifiable }) :: acc))
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
            let fwd =
              forward [] (Option.get version) current diff
                (S.serialise current |> H.digest)
            in
            (* XXX: Definitely doing something wrong here. *)
            root_to_current @ fwd
        | None -> root_to_current)

  let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs
  let latest t h = history t h |> last

  let add ?prev tbl c =
    match prev with
    | None ->
        let new_c = C.serialise c in
        let diff = S.Diff.diff S.empty new_c in
        let diff_hash = H.digest @@ S.serialise @@ S.Diff.serialise diff in
        let s = S.serialise S.empty in
        let prev = H.digest s in
        let hashed_diff = Hd.v ~verified:prev ~diff C.version |> Hd.serialise in
        let verifiable = H.to_raw_string prev |> H.digest in
        let hash = S.serialise new_c |> H.digest in
        Hashtbl.add tbl.prev diff_hash (S.of_string @@ H.to_hex prev);
        Hashtbl.add tbl.diffs diff_hash hashed_diff;
        Hashtbl.add tbl.prev hash (S.of_string @@ H.to_hex diff_hash);
        { content = hash; verifiable }
    | Some hash ->
        let actual = Hashtbl.find tbl.prev hash in
        let diff = Hashtbl.find tbl.diffs (H.of_hex @@ S.to_string actual) in
        let root, hashed_diffs =
          backward tbl ~prev:(Hashtbl.find tbl.prev hash) ~diff:(Some diff)
        in
        let _prev_version, prev = fast_forward root hashed_diffs in
        let prev_serialised = S.normalise prev in
        let prev_hash = prev_serialised |> S.serialise |> H.digest in
        let prev_hd = Hd.deserialise diff in
        let content = C.serialise c in
        let latest_diff = S.Diff.diff prev_serialised content in
        Hashtbl.add tbl.diffs prev_hash
          (Hd.serialise
          @@ Hd.v ?prev:(Hd.prev prev_hd) ~diff:latest_diff
               ~verified:(Hd.verified prev_hd) C.version);
        let verifiable = H.digest (H.to_raw_string @@ Hd.verified prev_hd) in
        let new_hash = H.digest (S.serialise content) in
        Hashtbl.add tbl.prev new_hash (S.of_string @@ H.to_hex prev_hash);
        { content = new_hash; verifiable }

  let dump t =
    let prevs = Hashtbl.to_seq t.prev |> List.of_seq in
    let store =
      List.map
        (fun (ca, prev) ->
          ( ca,
            H.of_hex @@ S.to_string prev,
            Option.bind (Hashtbl.find_opt t.diffs ca) (fun v ->
                Some (Hd.deserialise v)) ))
        prevs
    in
    List.iter
      (fun (ca, prev, s) ->
        Fmt.pr "%a: (%a) %s@." (H.pp ~short:true) ca (H.pp ~short:true) prev
          (match s with
          | Some s -> S.serialise @@ Hd.serialise s
          | None -> "none"))
      store
end

module SHA256 = struct
  type t = string

  let digest s = Digestif.SHA256.(digest_string s |> to_raw_string)
  let to_hex v = Digestif.SHA256.(to_hex @@ of_raw_string v)
  let of_hex v = Digestif.SHA256.(to_raw_string @@ consistent_of_hex v)
  let to_raw_string s = s

  let pp ?(short = false) ppf t =
    if short then Format.pp_print_string ppf (String.sub (to_hex t) 0 7)
    else Format.pp_print_string ppf (to_hex t)
end
