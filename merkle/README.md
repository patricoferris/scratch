# Merkle Proofs in Irmin

Quickly exploring the feasability of using merkle proofs in Irmin for public, private and auditible data.

The idea here was instead of keeping track in the data itself the connections to the private and audit data,
the shape of the Irmin store could do that for us. First we set up some dummy types:

<!-- $MDX file=main.ml,part=types -->
```ocaml
module Private = struct
  type t = { name: string; age : int; favourite_books : string list }[@@deriving irmin]
end

module Audit = struct
  type t = { name : string; favourite_books : string list }[@@deriving irmin]
end

module Public = struct
  type t = { number_of_favourite_books : int }[@@deriving irmin]
end
```

Next we enforce rules on how paths are constructed in our store. Concretely, users must provide a label for their
data at to whether or not it is private, audit or public data.

<!-- $MDX file=main.ml,part=paths -->
```ocaml
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
```

It isn't perfect as people may still use `v` to make bad paths, but that's okay for now. Now we store some values
and extract a proof!

<!-- $MDX file=main.ml,part=proof -->
```ocaml
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
```

And if we run the code:

```sh
$ ./main.exe
2ae278dd2770d05517bfb97329a1a148fb6af5530dbe1430fcacb45079c0c3645e21a9d7900d5123a3ca4419a009173d082d1716ba7ac3645d9bf15dc2b465f4{"Node":[["a",{"Node":[["audit",{"Blinded_contents":["485d4eea2f8eae1804b61b19f2c65413de7b16deea6dd24886f2fbb3592f1b56dd0e8dbec2494de22d5ab5a7927460eaeb2ed7e24aa34104b3cace96b424da18",{}]}],["private",{"Blinded_contents":["c2bef02c1259aed2249bbe155a87d82ba67dfbd08cd736abdefb07d24adec8fd58bce67526fb6f891a1ff469594c53d9246a587d18238553adbc5a6c53bdf16a",{}]}],["public",{"Contents":[{"Public":{"number_of_favourite_books":1}},{}]}]]}]]}
```
