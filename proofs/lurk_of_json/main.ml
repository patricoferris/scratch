(* A simple, probably partially broken binary for converting
   arbitrary JSON into lurk expressions *)
open Sexplib.Conv

let str = Format.asprintf

(* A little bit of lurk in OCaml. Amazing how much you can encode
  with just lists of strings and ints ^^ *)
module Lurk = struct
  type t =
    | Nil
    | Cons of t * t
    | String of string
    | Int of int

  let nil = Nil
  let cons e v = Cons (e, v)
  let string s = String s
  let int i = Int i

  let list lst = List.fold_left (fun acc v -> cons v acc) nil (List.rev lst)

  let to_string t =
    let rec to_string = function
        | Nil -> "nil"
        | Cons (a, b) -> str "(cons %s %s)" (to_string a) (to_string b)
        | String s -> str {|"%s"|} s
        | Int i -> string_of_int i
    in
    str "%s" (to_string t)
end

module Private = struct

  type flight = {
    timestamp : string;
    origin : string;
    destination : string;
  }[@@deriving of_yojson]

  let flight_to_lurk t =
    let open Lurk in
    let timestamp = cons (string "timestamp") (string t.timestamp) in
    let origin = cons (string "origin") (string t.origin) in
    let destination = cons (string "destination") (string t.destination) in
    list [ timestamp; origin; destination ]

  type t = {
    name : string;
    age : int;
    flights : flight list;
  }[@@deriving of_yojson]

  let to_lurk t =
    let open Lurk in
    let name = cons (string "name") (string t.name) in
    let age = cons (string "age") (int t.age) in
    let flights = List.map flight_to_lurk t.flights |> list in
    list [ name; age; cons (string "flights") flights ]
end

module Audit = struct
  type flight = {
    timestamp : string;
    origin : string;
    destination: string;
  }[@@deriving sexp, yojson]

  type t = {
    flights : flight list;
  }[@@deriving sexp, yojson]
end

module Public = struct
  type t = {
    total_flights : int;
  }[@@deriving sexp, yojson]
end

let () =
  In_channel.with_open_text Sys.argv.(2) @@ fun ic ->
  match Sys.argv.(1) with
  | "private" ->
    let json = Yojson.Safe.from_channel ic in
    let pr = Private.of_yojson json |> Result.get_ok in
    Private.to_lurk pr |> fun l ->
    Out_channel.output_string stdout (Lurk.to_string l)
  | "audit" ->
    let sexp = Sexplib.Sexp.input_sexp ic in
    let audit = Audit.t_of_sexp sexp in
    Yojson.Safe.to_channel stdout (Audit.to_yojson audit)
  | "public" ->
    let sexp = Sexplib.Sexp.input_sexp ic in
    let audit = Public.t_of_sexp sexp in
    Yojson.Safe.to_channel stdout (Public.to_yojson audit)
  | "function" ->
    (* fcomm should probably provide this... *)
    let lurk = In_channel.input_all ic |> String.trim |> String.split_on_char '\n' |> String.concat " " in
    let json = `Assoc [ "fun", `Assoc [ "Source", `String lurk; ]; "secret", `Null; "commitment", `Null ] in
    Yojson.Safe.to_channel stdout json
  | cmd -> failwith (Format.asprintf "Unknown command %s try private, audit, public" cmd)