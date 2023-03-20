(* $MDX part-begin=types *)
module Private = struct
  type t = { name: string; age : int; favourite_books : string list }[@@deriving irmin]
end

module Audit = struct
  type t = { name : string; favourite_books : string list }[@@deriving irmin]
end

module Public = struct
  type t = { number_of_favourite_books : int }[@@deriving irmin]
end
(* $MDX part-end *)

module Contents = struct
  type t = Private of Private.t | Audit of Audit.t | Public of Public.t [@@deriving irmin]

  let merge = Irmin.Merge.(option @@ default t)
end

(* $MDX part-begin=paths *)
module Kind = struct
  type t = Private | Audit | Public [@@deriving irmin]

  let to_string = function
    | Private -> "private"
    | Audit -> "audit"
    | Public -> "public"
end

module Paths : sig
  type step
  type t
  include Irmin.Path.S with type step := step and type t := t

  val make : Kind.t -> string list -> t
end = struct
  include Irmin.Path.String_list

  let make kind path = path @ [ Kind.to_string kind ]
end
(* $MDX part-end *)

module type Schema =
  Irmin.Schema.Extended
    with type Hash.t = Irmin.Schema.default_hash
     and type Branch.t = string
     and type Info.t = Irmin.Info.default
     and type Metadata.t = unit
     and type Path.step = Paths.step
     and type Path.t = Paths.t

module Make (C : Irmin.Contents.S) : Schema with module Contents = C = struct
  open Irmin
  module Hash = Hash.BLAKE2B
  module Info = Info.Default
  module Branch = Branch.String
  module Path = Paths
  module Metadata = Metadata.None
  module Contents = C
  module Node = Node.Generic_key.Make (Hash) (Paths) (Metadata)
  module Commit = Commit.Generic_key.Make (Hash)
end

module Schema = Make (Contents)
module Store = Irmin_mem.Make (Schema)

(* $MDX part-begin=proof *)
let store_with_proof repo path (private_data : Private.t) =
  let private_path = Paths.make Kind.Private path in
  let audit = Audit.{ name = private_data.name; favourite_books = private_data.favourite_books } in
  let audit_path = Paths.make Kind.Audit path in
  let public = Public.{ number_of_favourite_books = List.length private_data.favourite_books } in
  let public_path = Paths.make Kind.Public path in
  let empty = Store.Tree.empty () in
  let tree = Store.Tree.add empty public_path (Public public) in
  let tree = Store.Tree.add tree audit_path (Audit audit) in
  let tree = Store.Tree.add tree private_path (Private private_data) in
  let kinded_key =
    Store.Backend.Repo.batch repo (fun rw_contents_store rw_node_store _rw_commit_store ->
      Store.save_tree repo rw_contents_store rw_node_store tree)
  in
  let node_key = match kinded_key with `Node k -> k | _ -> failwith "Expected node key" in
  (Irmin.Type.pp Store.node_key_t) Fmt.stdout node_key;
  node_key

let visit_public_tree tree =
  let (_ : Contents.t option) = Store.Tree.find tree (Paths.make Kind.Public [ "a" ]) in
  (Store.Tree.empty (), `Success)

let proof repo key =
  let p, `Success = Store.Tree.produce_proof repo (`Node key) visit_public_tree in
  p
(* $MDX part-end *)

let pp_merkle_proof = Irmin.Type.pp Store.Tree.Proof.tree_t

let () =
  Eio_main.run @@ fun _ ->
  let config = Irmin_mem.config () in
  let repo = Store.Repo.v config in
  let key = store_with_proof repo [ "a" ] Private.{ name = "Alice"; age = 42; favourite_books = [ "Real World OCaml" ]} in
  let proof = proof repo key in
  pp_merkle_proof Fmt.stdout (Store.Tree.Proof.state proof)