include Hdiff_intf

module Make (H : Hash.S) (S : Serialiser.S) = struct
  type t = {
    version : Version.t;
    prev : H.t option;
    verified : H.t;
    diff : S.Diff.t option;
  }

  type hash = H.t
  type serial = S.t
  type diff = S.Diff.t

  module V = Version.Make (S)

  let v ?prev ?diff ~verified version = { version; prev; verified; diff }
  let prev t = t.prev
  let verified t = t.verified
  let diff t = t.diff

  let diff_exn t =
    match t.diff with Some diff -> diff | _ -> failwith "No diff found"

  let version t = t.version

  let serialise t =
    S.of_dict
      [
        ("version", V.serialise t.version);
        ( "prev",
          match t.prev with
          | None -> S.null
          | Some prev -> S.of_string (H.to_hex prev) );
        ("verified", S.of_string (H.to_hex t.verified));
        ( "diff",
          match t.diff with
          | None -> S.null
          | Some diff -> S.Diff.serialise diff );
      ]

  let deserialise s =
    match S.to_dict s with
    | [
     ("version", version); ("prev", hex); ("verified", ver); ("diff", diffs);
    ] ->
        let prev =
          if S.equal S.null hex then None else Some (H.of_hex @@ S.to_string hex)
        in
        let diff =
          if S.equal S.null diffs then None else Some (S.Diff.deserialise diffs)
        in
        {
          version = V.deserialise version;
          prev;
          verified = H.of_hex @@ S.to_string ver;
          diff;
        }
    | _ -> invalid_arg ("Failed to deserialise hashed diff " ^ S.serialise s)
end
