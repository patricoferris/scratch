module J = Store.Serialiser.Json

module Private = struct
  type t = { flights : flight list; tx : string option }
  and flight = { origin : string; dest : string }

  let version = Store.Version.{ major = 0; minor = 0; patch = 0 }

  type serial = J.t

  let equal = Stdlib.( = )

  let serialise t =
    let flight f =
      J.normalise
      @@ `Assoc [ ("origin", `String f.origin); ("dest", `String f.dest) ]
    in
    let tx = match t.tx with Some tx -> `String tx | None -> `Null in
    let v =
      `Assoc [ ("flights", `List (List.map flight t.flights)); ("tx", tx) ]
    in
    J.normalise v

  let deserialise_flight = function
    | `Assoc [ ("dest", `String dest); ("origin", `String origin) ] ->
        { origin; dest }
    | _ -> invalid_arg "Flight!"

  let get_string = function `String s -> s | _ -> invalid_arg "not a string"

  let deserialise = function
    | `Assoc [ ("flights", `List flights); ("tx", tx) ] ->
        let tx = if J.equal `Null tx then None else Some (get_string tx) in
        { flights = List.map deserialise_flight flights; tx }
    | v -> invalid_arg ("flights " ^ J.serialise v)
end

module Json_sha256_mem = Store.Mem (J) (Store.SHA256)
module Contents = Store.Contents.Make (J) (Private)

module Private_store = struct
  include Json_sha256_mem (Contents)

  let transaction_complete t hash =
    match latest t hash.content with
    | Some (_, serial, _) -> Option.is_some @@ (Contents.deserialise serial).tx
    | None -> false
end

module Global_store = struct
  type commitment = { tid : int; hash : Private_store.hash }
  and t = commitment list

  let v ~tid hash = { tid; hash }
  let store : t ref = ref []

  let add =
    let tid = ref 0 in
    fun ?(mock_fail = false) hash ->
      if mock_fail then None
      else (
        incr tid;
        store := v ~tid:!tid hash :: !store;
        Some !tid)

  let dump () =
    List.iter
      (fun { tid; hash } -> Fmt.pr "%i: %s\n" tid (Store.SHA256.to_hex hash))
      !store
end

let transact ~mock_fail store (hash : Private_store.hashes) =
  match
    ( Private_store.find store hash.content,
      Global_store.add ~mock_fail hash.content )
  with
  | Some v, Some tid ->
      ignore
        (Private_store.add ~prev:hash.content store
           { v with tx = Some (string_of_int tid) })
  | _ -> failwith "Transaction failed or item doesn't exist"

let () =
  let store = Private_store.empty () in
  let first =
    Private_store.add store
      Private.{ flights = [ { origin = "BFS"; dest = "LHR" } ]; tx = None }
  in
  let second =
    Private_store.add store
      Private.{ flights = [ { origin = "LHR"; dest = "TLS" } ]; tx = None }
  in
  transact ~mock_fail:false store first;
  (try transact ~mock_fail:true store second with _ -> ());
  assert (Private_store.transaction_complete store first);
  Global_store.dump ()
