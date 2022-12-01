module type S = sig
  type t
  (** The type for diffs with hashes *)

  type hash
  (** The hash type *)

  type diff
  (** The type for diffs *)

  val v : prev:hash -> verified:hash -> diff -> t
  (** A constructor for hdiffs *)

  type serial
  (** The serialisation type *)

  val prev : t -> hash
  (** The previous version hash *)

  val verified : t -> hash
  (** A hash that can be used to verify *)

  val diff : t -> diff
  (** The diff *)
end

module type Intf = sig
  module type S = S

  module Make : functor (H : Hash.S) (Ser : Serialiser.S) -> sig
    include
      S with type hash = H.t and type serial = Ser.t and type diff = Ser.Diff.t

    include Serialiser.Serialisable with type t := t and type serial = Ser.t
  end
end
