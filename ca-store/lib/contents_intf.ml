module type S = sig
  type t
  type serial

  val serialise : t -> serial
  val deserialise : serial -> t
  val equal : t -> t -> bool
  val version : Version.t
end

module type Versioned = sig
  include S

  val wrap : Version.t -> t -> serial
  val unwrap : serial -> Version.t * serial
  val unsafe_unwrap : serial -> Version.t * t
end

module type Intf = sig
  module type S = S
  module type Versioned = Versioned

  module Make : functor (S : Serialiser.S) (C : S with type serial = S.t) ->
    Versioned with type t = C.t and type serial = S.t
end
