module type S = sig
  type t

  val major : t -> int
  val minor : t -> int
  val patch : t -> int
  val compare : t -> t -> bool
end

module type Maker = functor (Ser : Serialiser.S) -> sig
  include S
  include Serialiser.Serialisable with type t := t and type serial = Ser.t
end

module type Intf = sig
  module type S = S

  include S
  module Make : Maker
end
