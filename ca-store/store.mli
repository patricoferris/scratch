module Version : sig
  type t = { major : int; minor : int; patch : int }
end

val consistent_yojson : Yojson.Basic.t -> Yojson.Basic.t

module type Contents = sig
    type t
    (** A type of contents *)
  
    val version : Version.t
    (** The version for this module *)
  
    val serialise : t -> Yojson.Basic.t
    (** Consistent serialiser *)

    val deserialise : Yojson.Basic.t -> t
    (** Consistent deserialiser *)
  
    val equal : t -> t -> bool
    (** Equality for content types *)
  end

module type Hash = sig
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
  end

module type Store = functor (H : Hash) (C : Contents) -> sig
  type t = (H.t, string) Hashtbl.t
  (** A store *)

  val empty : unit -> t
  (** The empty store *)

  val add : ?prev:H.t -> t -> C.t -> H.t option
  (** [add t c] adds content [c] to store [t] if it doesn't already exist.
      If you bump the version you can specify the previous hash to migrate. *)

  val find : t -> H.t -> C.t option
  (** [find t hash] does a content-addressed lookup in [t] for [hash] and
      might return the contents if it exists.
        
      Can raise an error if there is a version mismatch. *)

  val find_raw : t -> H.t -> (Version.t * string) option
  (** Like {! find} but returns the raw stored value *)

  val latest : t -> H.t -> (Version.t * string * H.t) option
  (** [latest t hash] is like {! find} except it finds the latest version of the
      contents if any. *)

  val dump : t -> unit
end

module Mem : Store 
module SHA256 : Hash with type t = string