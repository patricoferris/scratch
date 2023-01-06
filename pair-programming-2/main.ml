let check_ok = function
  | `Ok -> ()
  | `Partial -> failwith "Not supposed to be partial!"

let () =
  let rec loop d e = 
    match Jsonm.decode d with
    | `Lexeme v ->
      check_ok (Jsonm.encode e (`Lexeme v));
      loop d e
    | `End -> check_ok (Jsonm.encode e `End)
    | `Error e ->
      let ((r1, r2), (r3, r4)) = Jsonm.decoded_range d in
      Format.printf "Error(%i, %i, %i, %i): %a" r1 r2 r3 r4 Jsonm.pp_error e
    | `Await -> assert false
  in
  (* let decoder = Jsonm.decoder (`String "[\"Hello\", \"World\"]") in *)
  let buffer = Buffer.create 128 in
  let encoder = Jsonm.encoder (`Buffer buffer) in
  let () =
    In_channel.with_open_text "example.json" @@ fun inc ->
    let decoder = Jsonm.decoder (`Channel inc) in
    loop decoder encoder
  in
  print_endline (Buffer.contents buffer)

