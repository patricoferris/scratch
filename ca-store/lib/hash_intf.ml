module type S = sig
  type t
  (** A hash *)

  val digest : bool -> string -> t
  (** Digest a string *)

  val root : t -> bool
  (** Hashed a root or not *)

  val to_hex : t -> string
  (** Convert to a hex string *)

  val of_hex : string -> t
  (** Of a hex string *)

  val pp : Format.formatter -> t -> unit
end

module type Intf = sig
  module type S = S
end
