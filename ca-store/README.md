# Content-addressable Store with Version Migrations

<!-- TOC -->

- [Introduction](#introduction)
    - [Setup](#setup)
    - [Basic Store Interactions](#basic-store-interactions)
    - [Getting the Latest Version](#getting-the-latest-version)
- [External Transaction](#external-transaction)

<!-- /TOC -->

## Introduction
### Setup

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

### Basic Store Interactions

Now we perform some operations to show off our fancy store. First we create an empty
store and add a new person at version 0 to it.

```ocaml
# let s = Store0.empty ();;
val s : Store0.t = <abstr>
# let p1 = Person_v0.{ name = "Alice" };;
val p1 : Store0.content = {Person_v0.name = "Alice"}
# let h = Store0.add s p1;;
val h : Store2.hash = <abstr>
```

Let's inspect the state of the store and assert we still have a content addressed store.

```ocaml
# Store0.dump s;;
3cba1e3: {"version":{"major":0,"minor":0,"patch":0},"prev":"972f698e90f92ae3c0ec5c6faff3217156c8f592b55322e92899e91c967620a4","verified":"44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a","diff":null}
972f698: {"version":{"major":0,"minor":0,"patch":0},"prev":null,"verified":"44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a","diff":[{"add_field":"name","diff":"Alice"}]}
- : unit = ()
# assert (Option.get @@ Store0.find s h = p1);;
- : unit = ()
```

Now what if we bump the version of our person to include an age.

```ocaml
# let h' =
  Store1.add ~prev:h s Person_v0_0_1.{ name = "Alice"; age = 42 };;
val h' : Store2.hash = <abstr>
# Store1.dump s;;
3cba1e3: {"version":{"major":0,"minor":0,"patch":0},"prev":"972f698e90f92ae3c0ec5c6faff3217156c8f592b55322e92899e91c967620a4","verified":"44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a","diff":[{"add_field":"age","diff":42},{"no_diff":"name"}]}
9e5c9e6: {"version":{"major":0,"minor":0,"patch":1},"prev":"3cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a","verified":"3cba1e3cf23c8ce24b7e08171d823fbd9a4929aafd9f27516e30699d3a42026a","diff":null}
972f698: {"version":{"major":0,"minor":0,"patch":0},"prev":null,"verified":"44136fa355b3678a1146ad16f7e8649e94fb4fc21fe77e8310c060f61caaff8a","diff":[{"add_field":"name","diff":"Alice"}]}
- : unit = ()
```

### Getting the Latest Version

If we only have the hash for the old value of the person, we can still get the latest value from the store!

```ocaml
# match Store0.latest s h with
  | Some (_v, l, h'') -> (
      Fmt.pr "\nLatest value: %s (%s)\n" (J.serialise l) (Store.SHA256.to_hex h'');
      match Store1.find_raw s h'' with
      | Some (_, _) -> assert (h' = h'')
      | _ -> assert false
  )
  | None -> assert false;;
Latest value: {"age":42,"name":"Alice"} (9e5c9e6bd593b5c17c0aeb57456a443bceb5d8c7582233f93454199f9d9b0e93)
- : unit = ()
```

## External Transaction

Sometimes you want to commit to a value. That is to say, we tell everyone the hash of some 
private data and this prevents us from tampering with it in the future. When it comes to
auditing the data, auditors can be sure the data we committed to is indeed that data.

However, where do we public commit to this data. Perhaps a distributed ledger of some sort.
It would be great if we could then associate the successful recording of our commitment in
the private store. This would mean either having a separate store or mutating our original
private data, but this would break the hash!

We can however do this withour store.

```ocaml
module Private = struct
  type t = { 
    flights : flight list;
    tx : string option
  } and flight = {
    origin : string;
    dest : string;
  }

  let version = Store.Version.{ major = 0; minor = 0; patch = 0 }

  type serial = J.t

  let equal = Stdlib.( = )

  let serialise t =
    let flight f = 
      J.normalise @@ `Assoc [
        "origin", `String f.origin;
        "dest", `String f.dest
       ] 
    in
    let tx = match t.tx with Some tx -> `String tx | None -> `Null in
    let v = `Assoc [ "flights", `List (List.map flight t.flights); "tx", tx ] in
    J.normalise v

  let deserialise_flight = function
    | `Assoc [ "dest", `String dest; "origin", `String origin ] -> { origin; dest }
    | _ -> invalid_arg "Flight!"

  let get_string = function `String s -> s | _ -> invalid_arg "not a string"

  let deserialise = function
    | `Assoc [ "flights", `List flights; "tx", tx ] -> 
      let tx = if J.equal `Null tx then None else Some (get_string tx) in
      { flights = List.map deserialise_flight flights; tx }
    | _ -> invalid_arg "flights"
end
```

Because we have used a `string option` for our transaction, the committed version of this is identical.

```ocaml
module Committed = Private
```

We can now construct our private store.

```ocaml
module Contents = Store.Contents.Make (J) (Private)
module Private_store = struct 
  include Json_sha256_mem (Contents)

  let transaction_complete t hash =
    match latest t hash with
    | Some (_, serial, _) -> Option.is_some @@ (Contents.deserialise serial).tx 
    | None -> false
end
```

In order to commit to our values, we'll create a global store where we record the hash of the private data.

```ocaml
module Global_store = struct
  type commitment = { tid : int; hash : Private_store.hash }
  and t = commitment list

  let store : t ref = ref []

  let add = 
    let tid = ref 0 in
    fun ?(mock_fail = false) hash ->
    if mock_fail then None else (incr tid; store := { tid = !tid; hash } :: !store; Some !tid)

  let dump () =
    List.iter (fun { tid; hash } -> Fmt.pr "%i: %s\n" tid (Store.SHA256.to_hex hash)) !store
end
```

And now let's add two new pieces of data to our private store.

```ocaml
# let store = Private_store.empty ();;
val store : Private_store.t = <abstr>
# let first = Private_store.add store Private.{ flights = [ { origin = "BFS"; dest = "LHR" } ]; tx = None };;
val first : Private_store.hash = <abstr>
# let second = Private_store.add store Private.{ flights = [ { origin = "LHR"; dest = "TLS" } ]; tx = None };;
val second : Private_store.hash = <abstr>
```

And let's commit to our first value and mock a failure for the second value. First we'll build a transact function which can do all of this with only the hash of a stored value.

```ocaml
let transact ~mock_fail hash =
  match Private_store.find store hash, Global_store.add ~mock_fail hash with
  | Some v, Some tid -> ignore (Private_store.add ~prev:hash store { v with tx = Some (string_of_int tid) })
  | _ -> failwith "Transaction failed or item doesn't exist"
```

And now we can perform the transactions.

```ocaml
# transact ~mock_fail:false first;;
- : unit = ()
# transact ~mock_fail:true second;;
Exception: Failure "Transaction failed or item doesn't exist".
```

With only the original commitment we can now check if the transaction went through.

```ocaml
# Private_store.transaction_complete store first;;
- : bool = true
```
