
module Name : sig 
    type t
    val v : ?nick:string -> string -> t
    val name : t -> string
    val nick : t -> string option
end

type t
(** The type for a person *)

val v : ?nick:string -> string -> int -> t
(** [v ?nick name age] builds a new person. *)

val age : t -> int
(** [age t] extracts the person [t]'s age. *)

val name : t -> Name.t
(** [name t] extracts the person [t]'s name. *)

val pp : Format.formatter -> t -> unit
(** Pretty print a person. *)

val to_file : string -> t -> unit
(** Write a person to file. *)