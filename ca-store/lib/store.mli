module type S = Store_intf.S

module Version = Version
module Serialiser = Serialiser
module Contents = Contents

type ('a, 'b) mem = { prev : ('a, 'b) Hashtbl.t; diffs : ('a, 'b) Hashtbl.t }

module Mem : functor
  (S : Serialiser.S)
  (H : Hash.S)
  (C : Contents.S with type serial = S.t)
  ->
  S
    with type hash = H.t
     and type content = C.t
     and type serial = C.serial constraint C.serial = S.t
     and type t = (H.t, C.serial) mem

module SHA256 : Hash.S
