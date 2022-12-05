module type S = sig
  type t
  (** A hash *)

  val digest : string -> t
  (** Digest a string *)

  val to_hex : t -> string
  (** Convert to a hex string *)

  val of_hex : string -> t
  (** Of a hex string *)

  val pp : ?short:bool -> Format.formatter -> t -> unit
end

module type Intf = sig
  module type S = S
end
