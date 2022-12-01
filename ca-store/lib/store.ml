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

  let serial_is_versioned s = try Some (Cs.unwrap s) with _ -> None
  let empty : unit -> t = fun () -> Hashtbl.create 128

  let backward tbl diff =
    let rec loop acc diff =
      match Option.is_some @@ serial_is_versioned diff with
      | true -> (Option.get @@ serial_is_versioned diff, List.rev acc)
      | false ->
          let diff = Hd.deserialise diff in
          loop (diff :: acc) (Hashtbl.find tbl (Hd.prev diff))
    in
    loop [ diff ] (Hashtbl.find tbl (Hd.prev diff))

  let find_raw tbl h =
    match Hashtbl.find_opt tbl h with
    | None -> None
    | Some c -> (
        match serial_is_versioned c with
        | Some (v, c) -> Some (v, c)
        | None ->
            let hashed_diff = Hd.deserialise c in
            let (v, root), diffs = backward tbl hashed_diff in
            let current =
              S.normalise
              @@ List.fold_left S.Diff.apply root (List.map Hd.diff diffs)
            in
            Some (v, current))

  let find tbl h =
    Option.map
      (fun (k, v) ->
        if Version.compare C.version k = 0 then C.deserialise v
        else failwith "Version mismatch")
      (find_raw tbl h)

  let latest tbl h =
    let rec forward (version : Version.t) json hash =
      match Hashtbl.find_opt tbl hash with
      | Some diff -> (
          match serial_is_versioned diff with
          | Some (v, c) -> (
              match
                Hashtbl.find_opt tbl
                  (H.digest false (S.serialise (S.normalise c)))
              with
              | None -> (v, c, hash)
              | Some diff ->
                  let hd = Hd.deserialise diff in
                  let next_json = S.Diff.apply c (Hd.diff hd) in
                  let next_hash = S.serialise c |> H.digest false in
                  if Option.is_none @@ Hashtbl.find_opt tbl next_hash then
                    (v, json, hash)
                  else forward v next_json next_hash)
          | None ->
              let hd = Hd.deserialise diff in
              let next_json = S.Diff.apply json (Hd.diff hd) in
              let next_hash = S.serialise next_json |> H.digest false in
              if Option.is_none @@ Hashtbl.find_opt tbl next_hash then
                (version, json, hash)
              else forward version next_json next_hash)
      | None -> (version, json, hash)
    in
    match Hashtbl.find_opt tbl h with
    | None -> None
    | Some d -> (
        match serial_is_versioned d with
        | Some (v, current) ->
            let hash = S.serialise (S.normalise current) |> H.digest true in
            Some (forward v current hash)
        | None ->
            let hashed_diff = Hd.deserialise d in
            let (version, root), diffs = backward tbl hashed_diff in
            let current =
              S.normalise
              @@ List.fold_left S.Diff.apply root (List.map Hd.diff diffs)
            in
            Some
              (forward version current (S.serialise current |> H.digest false)))

  let rec last = function
    | [] -> assert false
    | [ x ] -> x
    | _ :: rest -> last rest

  let add ?prev tbl c =
    let ser = C.serialise c |> S.normalise |> S.serialise in
    let st = Cs.wrap C.version c in
    match Option.bind prev (Hashtbl.find_opt tbl) with
    | None ->
        let hash = H.digest true ser in
        Hashtbl.add tbl hash st;
        hash
    | Some stored -> (
        match serial_is_versioned stored with
        | Some (v, old_c) ->
            if Version.compare C.version v <= 0 then
              failwith "Version too small!"
            else
              let new_c = C.serialise c in
              let diff = S.Diff.diff old_c new_c in
              let s = S.serialise (S.normalise old_c) in
              let prev = H.digest true s in
              let hashed_diff =
                Hd.v ~prev ~verified:prev diff |> Hd.serialise
              in
              let new_hash = H.digest false s in
              Hashtbl.add tbl new_hash hashed_diff;
              new_hash
        | _ ->
            let diff = Hd.deserialise stored in
            (* Need to check versions are okay! *)
            let (_version, root), hashed_diffs = backward tbl diff in
            let diffs = List.map Hd.diff hashed_diffs in
            let reconstructed =
              S.normalise @@ List.fold_left S.Diff.apply root diffs
            in
            let prev =
              last hashed_diffs |> Hd.serialise |> S.serialise |> H.digest false
            in
            let latest_diff = S.Diff.diff reconstructed (C.serialise c) in
            let v = Hd.v ~prev ~verified:prev latest_diff in
            let c = Hd.serialise v in
            let new_hash = H.digest false (S.serialise reconstructed) in
            Hashtbl.add tbl new_hash c;
            new_hash)

  let dump t =
    Hashtbl.iter (fun k s -> Fmt.pr "%s: %s@." (H.to_hex k) (S.serialise s)) t
end

module SHA256 = struct
  type t = string

  let root t = t.[0] = '0'

  let digest root s =
    let prefix = if root then "0" else "1" in
    prefix ^ Digestif.SHA256.(digest_string s |> to_raw_string)

  let to_hex v =
    String.make 1 v.[0]
    ^ Digestif.SHA256.(to_hex @@ of_raw_string String.(sub v 1 (length v - 1)))

  let of_hex v =
    String.make 1 v.[0]
    ^ Digestif.SHA256.(
        to_raw_string @@ consistent_of_hex String.(sub v 1 (length v - 1)))

  let pp ppf t = Format.pp_print_string ppf (to_hex t)
end
