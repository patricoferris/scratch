## Content-addressable Store with Version Migrations

Setting up some store content types. First a simple person with name.

```ocaml
module Person_v0 = struct
  type t = {
    name : string;
  }

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

  let version = Store.Version.{ major = 0; minor = 0; patch = 1 }

  let serialise t = 
    Store.consistent_yojson @@ `Assoc [ "name", `String t.name; "age", `Int t.age ]

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

  let version = Store.Version.{ major = 0; minor = 0; patch = 2 }

  let serialise t = 
    Store.consistent_yojson @@ `Assoc [ "name", `String t.name; "age", `Int t.age; "nick", `String t.nick ]

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
module Store0 = Store.Mem (Store.SHA256) (Person_v0)
module Store1 = Store.Mem (Store.SHA256) (Person_v0_0_1)
module Store2 = Store.Mem (Store.SHA256) (Person_v0_0_2)
```

Now we perform some operations to show off our fancy store.

 1. We add Alice to the store and dump the store to see the hash and the contents.
 2. We double check content-addressed lookup works, we dump to show we stored only the diff.
 3. Now we lookup the latest version of the original Alice we stored!

```ocaml
# let () =
  let s = Store0.empty () in
  Fmt.pr "<><><> STORE 0 <><><>\n";
  let p1 = Person_v0.{ name = "Alice" } in
  let h = Option.get @@ Store0.add s p1 in
  Store0.dump s;
  assert (Option.get @@ Store0.find s h = p1);
  Fmt.pr "\n<><><> STORE 1 <><><>\n";
  let h' = 
    Option.get @@ Store1.add ~prev:h s Person_v0_0_1.{ name = "Alice"; age = 42 }
  in
  Store1.dump s;
  (match Store1.latest s h with
    | Some (_v, l, h'') -> (
      Fmt.pr "\nLatest value: %s (%s)\n" l (Store.SHA256.to_hex h'');
      match Store1.find_raw s h'' with 
        | Some (_, v) -> 
          assert (h' = h'')
        | _ -> assert false
    )
    | None -> assert false);
    Fmt.pr "\n<><><> STORE 2 <><><>\n";
    let h' = 
      Option.get @@ Store2.add ~prev:h' s Person_v0_0_2.{ name = "Alice"; age = 43; nick = "A" }
    in
    Store2.dump s;;
Line 23, characters 9-11:
Warning 26 [unused-var]: unused variable h'.
<><><> STORE 0 <><><>
03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"version":{"major":0,"minor":0,"patch":0},"value":{"name":"Alice"}}

<><><> STORE 1 <><><>
13cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"prev":"03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a","diff":[{"no_diff":"name"},{"add_field":"age","diff":42}]}
03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"version":{"major":0,"minor":0,"patch":0},"value":{"name":"Alice"}}

Latest value: {"age":42,"name":"Alice"} (13cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a)

<><><> STORE 2 <><><>
19e5c9e6bd593b5c17c0aeb57456a443bceb5d8c7582233f93454199f9d9b0e93: {"prev":"1367fc3c3177622f571335781c3f20797c0fa8befe1c207ef4de856db4056951f","diff":[{"update_field":"age","diff":43},{"no_diff":"name"},{"add_field":"nick","diff":"A"}]}
13cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"prev":"03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a","diff":[{"no_diff":"name"},{"add_field":"age","diff":42}]}
03cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a: {"version":{"major":0,"minor":0,"patch":0},"value":{"name":"Alice"}}
```
