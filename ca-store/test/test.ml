let json = Alcotest.of_pp Yojson.Basic.pp

let diff =
  Alcotest.of_pp (fun ppf v ->
      Yojson.Basic.pp ppf (Store.Serialiser.Json.Diff.serialise v))

let test_diff () =
  let open Store.Serialiser in
  let v = `Assoc [ ("hello", `String "world") ] in
  let v' = `Assoc [ ("hello", `String "WORLD") ] in
  let d = Json.Diff.diff v v' in
  let d_expect : Json.Diff.t =
    List [ Update_field ("hello", `String "WORLD") ]
  in
  Alcotest.(check diff) "same diff" d_expect d

let test_null () =
  let open Store.Serialiser in
  let v = `Assoc [ ("hello", `Null) ] in
  let v' = `Assoc [ ("hello", `Int 42) ] in
  let d = Json.Diff.diff v v' in
  let d_expect : Json.Diff.t = List [ Update_field ("hello", `Int 42) ] in
  Alcotest.(check diff) "same null update" d_expect d

let test_removal () =
  let open Store.Serialiser in
  let v = `Assoc [ ("hello", `String "World") ] in
  let v' = `Assoc [] in
  let d = Json.Diff.diff v v' in
  let d_expect : Json.Diff.t =
    List [ Remove_field ("hello", `String "World") ]
  in
  Alcotest.(check diff) "removal" d_expect d

let test_nested () =
  let open Store.Serialiser in
  let v = `Assoc [ ("hello", `Assoc [ ("salut", `String "World") ]) ] in
  let v' = `Assoc [ ("hello", `Assoc [ ("salut", `String "WORLD") ]) ] in
  let d = Json.Diff.diff v v' in
  let d_expect : Json.Diff.t =
    List [ Nest ("hello", List [ Update_field ("salut", `String "WORLD") ]) ]
  in
  Alcotest.(check diff) "nested" d_expect d

let test_diff_apply () =
  let open Store.Serialiser in
  let v = `Assoc [ ("hello", `String "world") ] in
  let d : Json.Diff.t = List [ No_diff "hello" ] in
  let v' = Json.Diff.apply v d in
  Alcotest.(check json) "same json no diff" v v';
  let v2 = `Assoc [ ("hello", `String "WORLD") ] in
  let d : Json.Diff.t = List [ Update_field ("hello", `String "WORLD") ] in
  let v' = Json.Diff.apply v d in
  Alcotest.(check json) "same json captial" v2 v';
  let v3 =
    Json.normalise @@ `Assoc [ ("hello", `String "WORLD"); ("age", `Int 42) ]
  in
  let d : Json.Diff.t = List [ No_diff "hello"; Add_field ("age", `Int 42) ] in
  let v' = Json.normalise @@ Json.Diff.apply v2 d in
  Alcotest.(check json) "same json add field" v3 v';
  let null = Json.normalise @@ `Assoc [ ("nest", `Assoc [ ("age", `Null) ]) ] in
  let expect =
    Json.normalise @@ `Assoc [ ("nest", `Assoc [ ("age", `Int 42) ]) ]
  in
  let d : Json.Diff.t = List [ Nest ("nest", Update_field ("age", `Int 42)) ] in
  let v' = Json.normalise @@ Json.Diff.apply null d in
  Alcotest.(check json) "same json nest" expect v'

let test_diff_apply_null () =
  let open Store.Serialiser in
  let v = `Assoc [ ("hello", `String "world"); ("tx", `Null) ] in
  let expected = `Assoc [ ("hello", `String "world"); ("tx", `Int 42) ] in
  let d : Json.Diff.t =
    List [ No_diff "hello"; Update_field ("tx", `Int 42) ]
  in
  let v' = Json.Diff.apply v d in
  Alcotest.(check json) "same null update" expected v'

let () =
  Alcotest.run "store"
    [
      ( "diff_gen",
        [
          Alcotest.test_case "diff" `Quick test_diff;
          Alcotest.test_case "null" `Quick test_null;
          Alcotest.test_case "remove" `Quick test_removal;
          Alcotest.test_case "nested" `Quick test_nested;
        ] );
      ( "diff_apply",
        [
          Alcotest.test_case "diff" `Quick test_diff_apply;
          Alcotest.test_case "null" `Quick test_diff_apply_null;
        ] );
    ]
