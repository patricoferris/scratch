module type S = sig
  type t
  (** A serialisation format. *)

  val serialise : t -> string
  (** Serialise to a string. *)

  val deserialise : string -> t
  (** Deserialise from a string. *)

  val equal : t -> t -> bool
  (** Equality of serialisation formats. *)

  val empty : t
  (** Empty object like thing... *)

  val null : t
  val of_int : int -> t
  val to_int : t -> int
  val of_string : string -> t
  val to_string : t -> string

  val of_dict : (string * t) list -> t
  (** *)

  val to_dict : t -> (string * t) list
  (** *)

  val normalise : t -> t
  (** Normalises the serialisation format, such that if I have hashing
      algorithm [h] over strings then if [equal a b] then it should follow
      [hash (to_string (a)) = hash (to_string (b)) ]*)

  module Diff : Diff_intf.S with type serial = t
end

module type Serialisable = sig
  type t
  type serial

  val serialise : t -> serial
  val deserialise : serial -> t
end

module type Intf = sig
  module type Serialisable = Serialisable
  module type S = S
end
