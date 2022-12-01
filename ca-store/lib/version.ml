include Version_intf

type t = { major : int; minor : int; patch : int }

let compare a b =
  let maj = Int.compare a.major b.major in
  let min = Int.compare a.minor b.minor in
  let pat = Int.compare a.patch b.patch in
  if maj <> 0 then maj
  else if min <> 0 then min
  else if pat <> 0 then pat
  else 0

module Make (S : Serialiser_intf.S) = struct
  let serialise t =
    S.of_dict
      [
        ("major", S.of_int t.major);
        ("minor", S.of_int t.minor);
        ("patch", S.of_int t.patch);
      ]

  let deserialise t =
    match S.to_dict t with
    | [ ("major", major); ("minor", minor); ("patch", patch) ] ->
        Ok
          {
            major = S.to_int major;
            minor = S.to_int minor;
            patch = S.to_int patch;
          }
    | _ -> Error (`Msg "Malformed version!")
end
