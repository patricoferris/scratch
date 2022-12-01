module type S = sig
  type t
  (** The type of diffs *)

  type serial
  (** The serialisation type *)

  val diff : serial -> serial -> t
  (** Compute the diff between two serialisations *)

  val apply : serial -> t -> serial
  (** Apply a diff to a serialisation format *)

  val serialise : t -> serial
  (** Serialise a diff *)

  val deserialise : serial -> t
  (** Deserialise a diff *)
end
