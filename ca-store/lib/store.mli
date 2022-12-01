module type S = Store_intf.S

module Version = Version
module Serialiser = Serialiser
module Contents = Contents

module Mem : functor
  (S : Serialiser.S)
  (* (_ : sig
       val version : Version.t
     end) *)
(H : Hash.S)
  (C : Contents.S with type serial = S.t)
  ->
  S
    with type hash = H.t
     and type content = C.t
     and type serial = C.serial constraint C.serial = S.t
     and type t = (H.t, C.serial) Hashtbl.t

module SHA256 : Hash.S with type t = string
