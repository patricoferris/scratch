module J = Store.Serialiser.Json

module Person_v0 = struct
  type t = { name : string }
  type serial = J.t

  let version = Store.Version.{ major = 0; minor = 0; patch = 0 }
  let serialise t = `Assoc [ ("name", `String t.name) ]

  let deserialise = function
    | `Assoc [ ("name", `String name) ] -> { name }
    | _ -> failwith "Failed to deserialise person"

  let equal = Stdlib.( = )
end

module Person_v0_0_1 = struct
  type t = { name : string; age : int }
  type serial = J.t

  let version = Store.Version.{ major = 0; minor = 0; patch = 1 }

  let serialise t =
    J.normalise @@ `Assoc [ ("name", `String t.name); ("age", `Int t.age) ]

  let deserialise = function
    | `Assoc v -> (
        match (List.assoc_opt "name" v, List.assoc_opt "age" v) with
        | Some (`String name), Some (`Int age) -> { name; age }
        | _ -> failwith "Failed to deserialise person 2")
    | _ -> failwith "Failed to deserialise person 2"

  let equal = Stdlib.( = )
end

module Person_v0_0_2 = struct
  type t = { name : string; nick : string; age : int }
  type serial = J.t

  let version = Store.Version.{ major = 0; minor = 0; patch = 2 }

  let serialise t =
    J.normalise
    @@ `Assoc
         [
           ("name", `String t.name);
           ("age", `Int t.age);
           ("nick", `String t.nick);
         ]

  let deserialise = function
    | `Assoc v -> (
        match
          ( List.assoc_opt "name" v,
            List.assoc_opt "age" v,
            List.assoc_opt "nick" v )
        with
        | Some (`String name), Some (`Int age), Some (`String nick) ->
            { name; age; nick }
        | _ -> failwith "Failed to deserialise person 3")
    | _ -> failwith "Failed to deserialise person 3"

  let equal = Stdlib.( = )
end

module Json_sha256_mem = Store.Mem (J) (Store.SHA256)
module Name = Store.Contents.Make (J) (Person_v0)
module Store0 = Json_sha256_mem (Name)
module With_age = Store.Contents.Make (J) (Person_v0_0_1)
module Store1 = Json_sha256_mem (With_age)
module With_nick = Store.Contents.Make (J) (Person_v0_0_2)
module Store2 = Json_sha256_mem (With_nick)

let () =
  let s = Store0.empty () in
  Fmt.pr "<><><> STORE 0 <><><>\n";
  let p1 = Person_v0.{ name = "Alice" } in
  let h = Option.get @@ Store0.add s p1 in
  Store0.dump s;
  assert (Option.get @@ Store0.find s h = p1);
  Fmt.pr "\n<><><> STORE 1 <><><>\n";
  let h' =
    Option.get
    @@ Store1.add ~prev:h s Person_v0_0_1.{ name = "Alice"; age = 42 }
  in
  Store1.dump s;
  (match Store1.latest s h with
  | Some (_v, l, h'') -> (
      Fmt.pr "\nLatest value: %s (%s)\n" (J.serialise l)
        (Store.SHA256.to_hex h'');
      match Store1.find_raw s h'' with
      | Some (_, _) -> assert (h' = h'')
      | _ -> assert false)
  | None -> assert false);
  Fmt.pr "\n<><><> STORE 2 <><><>\n"
(* let h' =
     Option.get @@ Store2.add ~prev:h' s Person_v0_0_2.{ name = "Alice"; age = 43; nick = "A" }
   in
   Store2.dump s;; *)
