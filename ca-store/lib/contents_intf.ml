module type S = sig
  type t

  module Serial : Serialiser.S

  type version

  val version : version
  val serialise : t -> Serial.t
  val deserialise : Serial.t -> t
  val equal : t -> t -> bool
end

module type Versioned = sig
  include S with type version = Version.t

  val wrap : Version.t -> t -> Serial.t
  val unwrap : Serial.t -> Version.t * t
  val version : Version.t
end

module type Versioned_maker = functor
  (_ : sig
     val version : Version.t
   end)
  (C : S)
  -> Versioned with type t = C.t and type version = Version.t

module type Intf = sig
  module type S = S

  module Make : Versioned_maker
end
