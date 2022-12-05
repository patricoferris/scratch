include Serialiser_intf

type json_diff_simple =
  | Add_field of string * Yojson.Basic.t
  | Remove_field of string * Yojson.Basic.t
  | Update_field of string * Yojson.Basic.t
  | List of json_diff_simple list
  | Nest of string * json_diff
  | No_diff of string

and json_diff = json_diff_simple

module Json = struct
  type t = Yojson.Basic.t

  let empty = `Assoc []
  let null = `Null

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
        List.length a = List.length a'
        && List.for_all
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
    type t = json_diff

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
      | x ->
          Fmt.failwith "Failed to deserialise diff, not a list! %a"
            Yojson.Basic.pp x

    let is_not_object = function `Assoc _ -> false | #Yojson.Basic.t -> true

    let diff_update k v v' =
      match (v, v') with
      | `Null, u -> if u = `Null then No_diff k else Update_field (k, u)
      | `String s, (`String s' as u) ->
          if String.equal s s' then No_diff k else Update_field (k, u)
      | `Float s, (`Float s' as u) ->
          if Float.equal s s' then No_diff k else Update_field (k, u)
      | `Bool s, (`Bool s' as u) ->
          if Bool.equal s s' then No_diff k else Update_field (k, u)
      | `Int s, (`Int s' as u) ->
          if Int.equal s s' then No_diff k else Update_field (k, u)
      | `List v, (`List s' as u) ->
          if v = s' then No_diff k else Update_field (k, u)
      | _ -> No_diff k

    let _remove_duplicates diffs =
      List.fold_left
        (fun acc v -> if List.mem v acc then acc else v :: acc)
        [] diffs
      |> List.rev

    let get_obj = function
      | `Assoc assoc -> assoc
      | _ -> failwith "Not an object"

    let rec diff a b : t =
      let assoc_a = get_obj a in
      let assoc_b = get_obj b in
      let keys_a = U.keys a in
      let keys_b = U.keys b in
      let fields_to_remove =
        List.filter_map
          (fun k ->
            if List.mem k keys_b then None
            else Some (Remove_field (k, List.assoc k assoc_a)))
          keys_a
      in
      let fields_to_add =
        List.filter_map
          (fun k ->
            if List.mem k keys_a then None
            else Some (Add_field (k, List.assoc k assoc_b)))
          keys_b
      in
      let nested_and_updates =
        List.filter_map
          (fun k ->
            if List.mem k keys_a then
              let a_value = U.member k a in
              let b_value = U.member k b in
              if is_not_object b_value then Some (diff_update k a_value b_value)
              else Some (Nest (k, diff a_value b_value))
            else None)
          keys_b
      in
      List (fields_to_remove @ fields_to_add @ nested_and_updates)

    let rec apply_diff (json : Yojson.Basic.t) diff =
      match (json, diff) with
      | `Assoc _, Add_field (s, j) ->
          `Assoc [ (s, j) ]
          (* let did_update = ref false in
             let assoc =
               List.map
                 (fun (k, v) ->
                   if k = s && v = `Null then (
                     did_update := true;
                     (k, j))
                   else (k, v))
                 a
             in
             if !did_update then `Assoc assoc else `Assoc ((s, j) :: a) *)
      | `Assoc _, Remove_field (_, _) ->
          `Assoc [] (* `Assoc (List.filter (fun (k, _) -> s = k) a) *)
      | `Assoc a, Update_field (s, j) ->
          `Assoc
            (List.filter_map
               (fun (k, _) -> if k = s then Some (k, j) else None)
               a)
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
          `Assoc (List.concat assoc)
      | `Assoc a, Nest (s, diff) ->
          let a' =
            List.map
              (fun ((k, v) as t) -> if k = s then (k, apply_diff v diff) else t)
              a
          in
          `Assoc a'
      | `Assoc j, No_diff k ->
          `Assoc
            (List.filter_map
               (fun ((k', _) as t) -> if k = k' then Some t else None)
               j)
      | j, No_diff _ -> j
      | _ -> `Null

    let apply json diff : Yojson.Basic.t =
      match (json, diff) with
      | (`Assoc _ as v), diff ->
          let v = normalise @@ apply_diff v diff in
          v
      | _ -> failwith "Failed to apply diff."
  end
end
