module type S = sig
  type t
  (** A store *)

  type hash
  (** Hash type *)

  type content
  (** Content type *)

  type serial
  (** Serialisation type *)

  val empty : unit -> t
  (** The empty store *)

  val add : ?prev:hash -> t -> content -> hash
  (** [add t c] adds content [c] to store [t] if it doesn't already exist.
      If you bump the version you can specify the previous hash to migrate. *)

  val find : t -> hash -> content option
  (** [find t hash] does a content-addressed lookup in [t] for [hash] and
      might return the contents if it exists.
        
      Can raise an error if there is a version mismatch. *)

  val find_raw : t -> hash -> (Version.t * serial) option
  (** Like {! find} but returns the raw stored value *)

  val latest : t -> hash -> (Version.t * serial * hash) option
  (** [latest t hash] is like {! find} except it finds the latest version of the
      contents if any. *)

  val history : t -> hash -> (Version.t * serial * hash) list
  (** [history t hash] gets [hash]'s full history in the store. *)

  val dump : t -> unit
end
