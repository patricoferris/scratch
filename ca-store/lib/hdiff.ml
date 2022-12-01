include Hdiff_intf

module Make (H : Hash.S) (S : Serialiser.S) = struct
  type t = { prev : H.t; verified : H.t; diff : S.Diff.t }
  type hash = H.t
  type serial = S.t
  type diff = S.Diff.t

  let v ~prev ~verified diff = { prev; verified; diff }
  let prev t = t.prev
  let verified t = t.verified
  let diff t = t.diff

  let serialise t =
    S.of_dict
      [
        ("prev", S.of_string (H.to_hex t.prev));
        ("verified", S.of_string (H.to_hex t.verified));
        ("diff", S.Diff.serialise t.diff);
      ]

  let deserialise s =
    match S.to_dict s with
    | [ ("prev", hex); ("verified", ver); ("diff", diffs) ] ->
        {
          prev = H.of_hex @@ S.to_string hex;
          verified = H.of_hex @@ S.to_string ver;
          diff = S.Diff.deserialise diffs;
        }
    | _ -> invalid_arg ("Failed to deserialise hashed diff " ^ S.serialise s)
end
