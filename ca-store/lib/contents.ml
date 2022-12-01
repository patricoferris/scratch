include Contents_intf

module Make (S : Serialiser.S) (C : S with type serial = S.t) = struct
  include C
  module Ver = Version.Make (S)

  let version = C.version

  let wrap version c =
    S.of_dict [ ("version", Ver.serialise version); ("value", serialise c) ]

  let unwrap t =
    match S.to_dict t with
    | [ ("version", version); ("value", value) ] ->
        (Ver.deserialise version, value)
    | _ -> invalid_arg "Failed to deserialise"

  let unsafe_unwrap t =
    match S.to_dict t with
    | [ ("version", version); ("value", value) ] ->
        let v = Ver.deserialise version in
        if Version.compare C.version v = 0 then (v, deserialise value)
        else invalid_arg "Version mismatch"
    | _ -> invalid_arg "Failed to deserialise"
end
