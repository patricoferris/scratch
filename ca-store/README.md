## Content-addressable Store with Version Migrations

Before we begin, we will install a pretty printer for the hashes!

```ocaml
# #install_printer Store.SHA256.pp;;
```

We will be using the provided JSON serialiser.

```ocaml
module J = Store.Serialiser.Json
```

Next we set up some store content types. First a simple person with name.

```ocaml
module Person_v0 = struct
  type t = {
    name : string;
  }[@@deriving yojson]

  type serial = J.t

  let version = Store.Version.{ major = 0; minor = 0; patch = 0; }

  let serialise t = `Assoc [ "name", `String t.name ]

  let deserialise = function
   | `Assoc [ "name", `String name ] -> { name }
   | _ -> failwith "Failed to deserialise person"

  let equal = Stdlib.( = )
end
```

Then a new version of a person, but with an age too!

```ocaml
module Person_v0_0_1 = struct
  type t = {
    name : string;
    age : int
  }

  type serial = J.t

  let version = Store.Version.{ major = 0; minor = 0; patch = 1 }

  let serialise t = 
    J.normalise @@ `Assoc [ "name", `String t.name; "age", `Int t.age ]

  let deserialise = function
   | `Assoc v -> (
    match List.assoc_opt "name" v, List.assoc_opt "age" v with
    | Some (`String name), Some (`Int age) -> { name; age }
    | _ -> failwith "Failed to deserialise person 2"
   )
   | _ -> failwith "Failed to deserialise person 2"

  let equal = Stdlib.( = )
end
```

And one more for luck.

```ocaml
module Person_v0_0_2 = struct
  type t = {
    name : string;
    nick : string;
    age : int
  }

  type serial = J.t

  let version = Store.Version.{ major = 0; minor = 0; patch = 2 }

  let serialise t = 
    J.normalise @@ `Assoc [ "name", `String t.name; "age", `Int t.age; "nick", `String t.nick ]

  let deserialise = function
   | `Assoc v -> (
    match List.assoc_opt "name" v, List.assoc_opt "age" v, List.assoc_opt "nick" v with
    | Some (`String name), Some (`Int age), Some (`String nick) -> { name; age; nick }
    | _ -> failwith "Failed to deserialise person 3"
   )
   | _ -> failwith "Failed to deserialise person 3"

  let equal = Stdlib.( = )
end
```

After which we can set up two stores for each version of a person. They will actually share the same store later.

```ocaml
module Json_sha256_mem = Store.Mem (J) (Store.SHA256)
module Name = Store.Contents.Make (J) (Person_v0)
module Store0 = Json_sha256_mem (Name)
module With_age = Store.Contents.Make (J) (Person_v0_0_1)
module Store1 = Json_sha256_mem (With_age)
module With_nick = Store.Contents.Make (J) (Person_v0_0_2)
module Store2 = Json_sha256_mem (With_nick)
```

Now we perform some operations to show off our fancy store. First we create an empty
store and add a new person at version 0 to it.

```ocaml
# let s = Store0.empty ();;
val s : Store0.t = <abstr>
# let p1 = Person_v0.{ name = "Alice" };;
val p1 : Store0.content =
  {Person_v0.name =
    <printer Store.SHA256.pp raised an exception: Invalid_argument("invalid hash size")>}
# let h = Store0.add s p1;;
val h : string =
  03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a
```

Let's inspect the state of the store and assert we still have a content addressed store.

```ocaml
# Store0.dump s;;
03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"version":{"major":0,"minor":0,"patch":0},"value":{"name":"Alice"}}
- : unit = ()
# assert (Option.get @@ Store0.find s h = p1);;
- : unit = ()
```

Now what if we bump the version of our person to include an age.

```ocaml
# let h' =
  Store1.add ~prev:h s Person_v0_0_1.{ name = "Alice"; age = 42 };;
val h' : string =
  13cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a
# Store1.dump s;;
13cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"prev":"03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a","verified":"03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a","diff":[{"no_diff":"name"},{"add_field":"age","diff":42}]}
03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"version":{"major":0,"minor":0,"patch":0},"value":{"name":"Alice"}}
- : unit = ()
```

If we only have the hash for the old value of the person, we can still get the latest value from the store!

```ocaml
# match Store1.latest s h with
  | Some (_v, l, h'') -> (
      Fmt.pr "\nLatest value: %s (%s)\n" (J.serialise l) (Store.SHA256.to_hex h'');
      match Store1.find_raw s h'' with
      | Some (_, _) -> assert (h' = h'')
      | _ -> assert false
  )
  | None -> assert false;;
Latest value: {"age":42,"name":"Alice"} (13cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a)
- : unit = ()
```
