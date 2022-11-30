let rec consistent_yojson = function
  | `Assoc v -> 
    let assoc = List.stable_sort (fun (k, _) (k', _) -> String.compare k k') v in
    let assoc = List.map (fun (k, v) -> k, consistent_yojson v) assoc in
    `Assoc assoc
  | v -> v

let get_ok = function
  | Ok v -> v
  | Error (`Msg m) -> failwith m

module Version = struct
  type t = { major : int; minor : int; patch : int }

  let compare a b =
    let maj = Int.compare a.major b.major in
    let min = Int.compare a.minor b.minor in
    let pat = Int.compare a.patch b.patch in
    if maj <> 0 then maj
    else if min <> 0 then min
    else if pat <> 0 then pat
    else 0

  let to_json t : Yojson.Basic.t = `Assoc [ "major", `Int t.major; "minor", `Int t.minor; "patch", `Int t.patch ]

  let of_json = function
    | `Assoc [ "major", `Int major; "minor", `Int minor; "patch", `Int patch ] ->
      Ok { major; minor; patch } 
    | _ -> Error (`Msg "Malformed version!")
end

module type Contents = sig
  type t

  val version : Version.t

  val serialise : t -> Yojson.Basic.t
  val deserialise : Yojson.Basic.t -> t

  val equal : t -> t -> bool
end

module Diff = struct
  module J = struct
    type t = Yojson.Basic.t

    include Yojson.Basic.Util
  end

  let find k v = try Some (J.member k v) with _ -> None

  (* We can only ever store objects! *)
  type diff =
   | Add_field of string * J.t
   | Remove_field of string * J.t
   | Update_field of string * J.t
   | List of diff list
   | Nest of string * diff
   | No_diff of string

  and t = diff

  let rec diff_to_yojson = function
    | Add_field (s, j) -> `Assoc [ "add_field", `String s; "diff", j ]
    | Remove_field (s, j) -> `Assoc [ "remove_field", `String s; "diff", j ]
    | Update_field (s, j) -> `Assoc [ "update_field", `String s; "diff", j ]
    | List diffs -> `List (List.map diff_to_yojson diffs)
    | Nest (k, nest) -> nest_to_yojson k nest
    | No_diff s -> `Assoc [ "no_diff", `String s ]

  and nest_to_yojson s diffs = `Assoc [ "nest", `Assoc [ s, diff_to_yojson diffs ]]
  
  let to_yojson t : Yojson.Basic.t = diff_to_yojson t

  let rec diff_of_yojson = function
   | `Assoc [ "add_field", `String s; "diff", j ] -> Add_field (s, j)
   | `Assoc [ "remove_field", `String s; "diff", j ] -> Remove_field (s, j)
   | `Assoc [ "update_field", `String s; "diff", j ] -> Update_field (s, j)
   | `Assoc [ "nest", `Assoc [ s, diffs ]] -> Nest (s, (diff_of_yojson diffs))
   | `List diffs -> List (List.map diff_of_yojson diffs)
   | `Assoc [ "no_diff", `String s ] -> No_diff s
   | x -> 
    Fmt.failwith "Failed to deserialise diff! %a" Yojson.Basic.pp x

  let of_yojson : Yojson.Basic.t -> t = function 
  | `List diffs -> List (List.map diff_of_yojson diffs)
  | x -> Fmt.failwith "Failed to deserialise diff! %a" Yojson.Basic.pp x

  let is_not_object = function
   | `Assoc _ -> false
   | #Yojson.Basic.t -> true

  let diff_simple k v v' = match v, v' with
    | v, None | v, Some (`Null) -> Add_field (k, v)
    | `String s, Some ((`String s') as u) -> 
      if String.equal s s' then No_diff k else Update_field (k, u)
    | `Float s, Some ((`Float s') as u) -> 
        if Float.equal s s' then No_diff k else Update_field (k, u)
    | `Bool s, Some ((`Bool s') as u) -> 
      if Bool.equal s s' then No_diff k else Update_field (k, u)
    | `Int s, Some ((`Int s') as u) -> 
      if Int.equal s s' then No_diff k else Update_field (k, u)
    | `List v, Some ((`List s') as u) -> 
      if v = s' then No_diff k else Update_field (k, u)
    | _ -> No_diff k

  let remove_duplicates diffs =
    List.fold_left (fun acc v -> if List.mem v acc then acc else v :: acc) [] diffs
    |> List.rev

  let diff a b : t =
    let keys_a = J.keys a in
    let keys_b = J.keys b in
    let rec diff_a k =
      let v = Option.get @@ find k a in 
      let v' = find k b in
      if is_not_object v then diff_simple k v v'
      else begin
        let m = List.map diff_a (J.keys v) in
        List m
      end
    in
    let rec diff_b k =
      let v = Option.get @@ find k b in 
      let v' = find k a in
      if is_not_object v then diff_simple k v v'
      else begin
        let m = List.map diff_b (J.keys v) in
        List m
      end
    in 
    let diff_a = List.map (fun k -> diff_a k) keys_a in
    let diff_b = List.map (fun k -> diff_b k) keys_b in
    List (remove_duplicates (diff_a @ diff_b))

  let rec apply_diff (json : Yojson.Basic.t) diff =
    match json, diff with
    | `Assoc a, Add_field (s, j) -> `Assoc ((s, j) :: a)
    | `Assoc a, Remove_field (s, _) -> `Assoc (List.filter (fun (k, _) -> s = k) a)
    | `Assoc a, Update_field (s, j) -> `Assoc (List.map (fun ((k, _) as t) -> if k = s then (k, j) else t) a)
    | `List v, List diffs -> `List (List.map2 (fun v d -> apply_diff v d) v diffs)
    | (`Assoc _ as v), List diffs -> 
      let assoc = List.fold_left (fun acc d -> apply_diff v d :: acc) [] diffs in
      let assoc = List.filter_map (function `Assoc assoc -> Some assoc | _ -> None) assoc in
      `Assoc (remove_duplicates @@ List.concat assoc)
    | `Assoc a, Nest (s, diff) ->
      let a' = List.map (fun ((k, v) as t) -> if k = s then (k, apply_diff v diff) else t) a in
      `Assoc a'
    | j, No_diff _ -> j
    | _ -> `Null


  let apply json diff : Yojson.Basic.t = 
    match json, diff with
    | `Assoc _ as v, diff -> consistent_yojson @@ apply_diff v diff
    | _ -> failwith "Failed to apply diff."
end

module Versioned (C : Contents) = struct
  let wrap v c =
    let v = Version.to_json v in
    let c = C.serialise c in
    `Assoc [ "version", v; "value", c ]
    |> Yojson.Basic.to_string

  let _unwrap s = match Yojson.Basic.from_string s with 
  | `Assoc [ "version", v; "value", c ] -> 
    Result.map (fun v -> (v, C.deserialise c)) (Version.of_json v)
  | _ -> Error (`Msg "Failed to unwrap version and contents")
end

module type Hash = sig
  type t
  (** A hash *)

  val digest : bool -> string -> t
  (** Digest a string *)

  val root : t -> bool
  (** Hashed a root or not *)

  val to_hex : t -> string
  (** Convert to a hex string *)

  val of_hex : string -> t
  (** Of a hex string *)
end

module type Store = functor (H : Hash) (C : Contents) -> sig
  type t = (H.t, string) Hashtbl.t

  val empty : unit -> t

  val add : ?prev:H.t -> t -> C.t -> H.t option

  val find : t -> H.t -> C.t option

  val find_raw : t -> H.t -> (Version.t * string) option

  val latest : t -> H.t -> (Version.t * string * H.t) option

  val dump : t -> unit
end

module Mem (H : Hash) (C : Contents) = struct
  type t = (H.t, string) Hashtbl.t

  module Versioned_contents = Versioned (C)

  let is_versioned s = match Yojson.Basic.from_string s with
    | `Assoc [ "version", v; "value", value ] ->
      Option.map (fun v -> v, value) @@ Result.to_option @@ Version.of_json v
    | _ -> None

  module Hashed_diff = struct
    type t = {
      prev : H.t;
      diff : Diff.t;
    }

    let serialise t = `Assoc [
      "prev", `String (H.to_hex t.prev);
      "diff", Diff.to_yojson t.diff;
    ] |> Yojson.Basic.to_string

    let deserialise s = 
      match Yojson.Basic.from_string s with
      | `Assoc ["prev", `String hex; "diff", diffs ] ->
        Ok { prev = H.of_hex hex; diff = Diff.of_yojson diffs }
      | v -> Error (`Msg ("Failed to deserialise hashed diff " ^ Yojson.Basic.to_string v))
  end

  let empty : unit -> t = fun () -> Hashtbl.create 128

  let backward tbl diff =
    let rec loop acc diff = match Option.is_some @@ is_versioned diff with
      | true -> Option.get @@ is_versioned diff, List.rev acc
      | false ->
        let diff = get_ok @@ Hashed_diff.deserialise diff in
        loop (diff :: acc) (Hashtbl.find tbl diff.prev)
    in
    loop [ diff ] (Hashtbl.find tbl diff.prev)

  let find_raw_json tbl h = match Hashtbl.find_opt tbl h with
  | None -> None
  | Some c ->
    match is_versioned c with
    | Some (v, c) -> Some (v, c)
    | None ->
      let hashed_diff = get_ok @@ Hashed_diff.deserialise c in
      let (v, root), diffs = backward tbl hashed_diff in
      let current = consistent_yojson @@ List.fold_left Diff.apply root (List.map (fun v -> v.Hashed_diff.diff) diffs) in
      Some (v, current)

  let find_raw tbl h = Option.map (fun (k, v) -> (k, Yojson.Basic.to_string v)) (find_raw_json tbl h)

  let find (tbl : t) h =
    Option.map (fun (k, v) -> if Version.compare C.version k = 0 then C.deserialise v else failwith "Version mismatch") (find_raw_json tbl h)

  let latest tbl h =
    let rec forward (version : Version.t) json hash =
      match Hashtbl.find_opt tbl hash with
        | Some diff -> (
          match is_versioned diff with
          | Some (v, c) -> (
            match Hashtbl.find_opt tbl (H.digest false (Yojson.Basic.to_string (consistent_yojson c))) with
            | None -> v, Yojson.Basic.to_string c, hash
            | Some diff ->
              let hd = Hashed_diff.deserialise diff |> get_ok in
              let next_json = Diff.apply c hd.diff in
              let next_hash = Yojson.Basic.to_string c |> H.digest false in
              if Option.is_none @@ Hashtbl.find_opt tbl next_hash then v, Yojson.Basic.to_string json, hash else
              forward v next_json next_hash
          )
          | None ->
            let hd = Hashed_diff.deserialise diff |> get_ok in
            let next_json = Diff.apply json hd.diff in
            let next_hash = Yojson.Basic.to_string next_json |> H.digest false in
            if Option.is_none @@ Hashtbl.find_opt tbl next_hash then version, Yojson.Basic.to_string json, hash else
            forward version next_json next_hash
        )
        | None ->
          version, Yojson.Basic.to_string json, hash
    in
    match Hashtbl.find_opt tbl h with 
    | None -> None
    | Some d -> (
      match is_versioned d with
      | Some (v, current) -> 
        let hash = Yojson.Basic.to_string (consistent_yojson current) |> H.digest true in
        Some (forward v current hash)
      | None ->
        let hashed_diff = get_ok @@ Hashed_diff.deserialise d in
        let (version, root), diffs = backward tbl hashed_diff in
        let current = consistent_yojson @@ List.fold_left Diff.apply root (List.map (fun v -> v.Hashed_diff.diff) diffs) in
        Some (forward version current (Yojson.Basic.to_string current |> H.digest false))
    )

  let rec last = function
    | [] -> assert false
    | [ x ] -> x
    | _ :: rest -> last rest

  let add ?prev tbl c = 
    let ser = C.serialise c |> consistent_yojson |> Yojson.Basic.to_string in
    let st = Versioned_contents.wrap C.version c in
    match Option.bind prev (Hashtbl.find_opt tbl) with
    | None -> 
        let hash = H.digest true ser in
        Hashtbl.add tbl hash st;
        Some hash
      | Some stored ->
        match is_versioned stored with
          | Some (v, old_c) ->
            if Version.compare C.version v <= 0 then failwith "Version too small!"
            else begin
              let new_c = C.serialise c in
              let diff = Diff.diff old_c new_c in
              let s = Yojson.Basic.to_string (consistent_yojson old_c) in
              let hashed_diff = Hashed_diff.{ prev = H.digest true s; diff } |> Hashed_diff.serialise in
              let new_hash = H.digest false s in
              Hashtbl.add tbl new_hash hashed_diff;
              Some new_hash
            end
          | _ -> (
            match Hashed_diff.deserialise stored with
            | Error _ -> assert false
            | Ok diff ->
              (* Need to check versions are okay! *)
              let (_version, root), hashed_diffs = backward tbl diff in
              let diffs = List.map (fun v -> v.Hashed_diff.diff) hashed_diffs in
              let reconstructed = 
                consistent_yojson @@ List.fold_left Diff.apply root diffs
              in
              let prev = last hashed_diffs |> Hashed_diff.serialise |> H.digest false in
              let latest_diff = Diff.diff reconstructed (C.serialise c) in
              let v = Hashed_diff.{ prev; diff = latest_diff } in
              let c = Hashed_diff.serialise v in
              let new_hash = H.digest false (Yojson.Basic.to_string reconstructed) in
              Hashtbl.add tbl new_hash c;
              Some new_hash
          )

  let dump t = Hashtbl.iter (fun k s -> Fmt.pr "%s: %s@." (H.to_hex k) s) t
end

module SHA256 = struct
  type t = string

  let root t = t.[0] = '0' 

  let digest root s = 
    let prefix = if root then "0" else "1" in
    prefix ^ Digestif.SHA256.(digest_string s |> to_raw_string)

  let to_hex v = String.make 1 v.[0] ^ Digestif.SHA256.(to_hex @@ of_raw_string (String.(sub v 1 (length v - 1))))
  let of_hex v = String.make 1 v.[0] ^ Digestif.SHA256.(to_raw_string @@ consistent_of_hex (String.(sub v 1 (length v - 1))))
end


