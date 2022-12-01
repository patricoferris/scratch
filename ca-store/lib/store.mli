module type S = Store_intf.S

module Mem : functor (H : Hash.S) (C : Contents.Versioned) ->
  S
    with type hash = H.t
     and type content = C.t
     and type t = (H.t, C.Serial.t) Hashtbl.t

module SHA256 : Hash.S with type t = string
