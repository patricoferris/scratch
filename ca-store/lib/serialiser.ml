include Serialiser_intf

module Json = struct
  type t = Yojson.Basic.t

  module U = Yojson.Basic.Util

  let rec normalise = function
    | `Assoc v ->
        let assoc =
          List.stable_sort (fun (k, _) (k', _) -> String.compare k k') v
        in
        let assoc = List.map (fun (k, v) -> (k, normalise v)) assoc in
        `Assoc assoc
    | v -> v

  let serialise s = Yojson.Basic.to_string s
  let deserialise s = Yojson.Basic.from_string s

  let rec equal a b =
    match (normalise a, normalise b) with
    | `String s, `String s' -> String.equal s s'
    | `Int s, `Int s' -> Int.equal s s'
    | `Float s, `Float s' -> Float.equal s s'
    | `Bool s, `Bool s' -> Bool.equal s s'
    | `Null, `Null -> true
    | `List l, `List l' ->
        List.for_all (fun (v, v') -> equal v v') (List.combine l l')
    | `Assoc a, `Assoc a' ->
        List.for_all
          (fun ((k, v), (k', v')) -> String.equal k k' && equal v v')
          (List.combine a a')
    | _ -> false

  let of_dict assoc = `Assoc assoc
  let of_int i = `Int i
  let to_int = function `Int i -> i | _ -> invalid_arg "Not an integer!"
  let of_string i = `String i
  let to_string = function `String i -> i | _ -> invalid_arg "Not an string!"

  let to_dict = function
    | `Assoc assoc -> assoc
    | _ -> invalid_arg "Not a dictionary!"

  module Diff = struct
    type serial = t

    type diff =
      | Add_field of string * serial
      | Remove_field of string * serial
      | Update_field of string * serial
      | List of diff list
      | Nest of string * diff
      | No_diff of string

    and t = diff

    let rec diff_to_yojson = function
      | Add_field (s, j) -> `Assoc [ ("add_field", `String s); ("diff", j) ]
      | Remove_field (s, j) ->
          `Assoc [ ("remove_field", `String s); ("diff", j) ]
      | Update_field (s, j) ->
          `Assoc [ ("update_field", `String s); ("diff", j) ]
      | List diffs -> `List (List.map diff_to_yojson diffs)
      | Nest (k, nest) -> nest_to_yojson k nest
      | No_diff s -> `Assoc [ ("no_diff", `String s) ]

    and nest_to_yojson s diffs =
      `Assoc [ ("nest", `Assoc [ (s, diff_to_yojson diffs) ]) ]

    let serialise t : Yojson.Basic.t = diff_to_yojson t

    let rec diff_of_yojson = function
      | `Assoc [ ("add_field", `String s); ("diff", j) ] -> Add_field (s, j)
      | `Assoc [ ("remove_field", `String s); ("diff", j) ] ->
          Remove_field (s, j)
      | `Assoc [ ("update_field", `String s); ("diff", j) ] ->
          Update_field (s, j)
      | `Assoc [ ("nest", `Assoc [ (s, diffs) ]) ] ->
          Nest (s, diff_of_yojson diffs)
      | `List diffs -> List (List.map diff_of_yojson diffs)
      | `Assoc [ ("no_diff", `String s) ] -> No_diff s
      | x -> Fmt.failwith "Failed to deserialise diff! %a" Yojson.Basic.pp x

    let deserialise : Yojson.Basic.t -> t = function
      | `List diffs -> List (List.map diff_of_yojson diffs)
      | x -> Fmt.failwith "Failed to deserialise diff! %a" Yojson.Basic.pp x

    let is_not_object = function `Assoc _ -> false | #Yojson.Basic.t -> true

    let diff_simple k v v' =
      match (v, v') with
      | v, None | v, Some `Null -> Add_field (k, v)
      | `String s, Some (`String s' as u) ->
          if String.equal s s' then No_diff k else Update_field (k, u)
      | `Float s, Some (`Float s' as u) ->
          if Float.equal s s' then No_diff k else Update_field (k, u)
      | `Bool s, Some (`Bool s' as u) ->
          if Bool.equal s s' then No_diff k else Update_field (k, u)
      | `Int s, Some (`Int s' as u) ->
          if Int.equal s s' then No_diff k else Update_field (k, u)
      | `List v, Some (`List s' as u) ->
          if v = s' then No_diff k else Update_field (k, u)
      | _ -> No_diff k

    let remove_duplicates diffs =
      List.fold_left
        (fun acc v -> if List.mem v acc then acc else v :: acc)
        [] diffs
      |> List.rev

    let find k v = try Some (U.member k v) with _ -> None

    let diff a b : t =
      let keys_a = U.keys a in
      let keys_b = U.keys b in
      let rec diff_a k =
        let v = Option.get @@ find k a in
        let v' = find k b in
        if is_not_object v then diff_simple k v v'
        else
          let m = List.map diff_a (U.keys v) in
          List m
      in
      let rec diff_b k =
        let v = Option.get @@ find k b in
        let v' = find k a in
        if is_not_object v then diff_simple k v v'
        else
          let m = List.map diff_b (U.keys v) in
          List m
      in
      (* Needs to be fixed!!! *)
      let diff_a = List.map (fun k -> diff_a k) keys_a in
      let diff_b = List.map (fun k -> diff_b k) keys_b in
      let diff_b =
        List.filter (function Update_field _ -> false | _ -> true) diff_b
      in
      List (remove_duplicates (diff_a @ diff_b))

    let rec apply_diff (json : Yojson.Basic.t) diff =
      match (json, diff) with
      | `Assoc a, Add_field (s, j) -> `Assoc ((s, j) :: a)
      | `Assoc a, Remove_field (s, _) ->
          `Assoc (List.filter (fun (k, _) -> s = k) a)
      | `Assoc a, Update_field (s, j) ->
          `Assoc (List.map (fun ((k, _) as t) -> if k = s then (k, j) else t) a)
      | `List v, List diffs ->
          `List (List.map2 (fun v d -> apply_diff v d) v diffs)
      | (`Assoc _ as v), List diffs ->
          let assoc =
            List.fold_left (fun acc d -> apply_diff v d :: acc) [] diffs
          in
          let assoc =
            List.filter_map
              (function `Assoc assoc -> Some assoc | _ -> None)
              assoc
          in
          `Assoc (remove_duplicates @@ List.concat assoc)
      | `Assoc a, Nest (s, diff) ->
          let a' =
            List.map
              (fun ((k, v) as t) -> if k = s then (k, apply_diff v diff) else t)
              a
          in
          `Assoc a'
      | j, No_diff _ -> j
      | _ -> `Null

    let apply json diff : Yojson.Basic.t =
      match (json, diff) with
      | (`Assoc _ as v), diff -> normalise @@ apply_diff v diff
      | _ -> failwith "Failed to apply diff."
  end
end
