(* Simple parallel URL fetchers

   Crux of the problem: exposing a map-reduce like model to the user
   with scheduler-based parallelism. Key insight, only the scheduler
   knows enough information to intelligently do things in parallel... maybe

*)

(* Attempt 1

   Initially we just went with raising effects, but this is no good because
   we don't know enough about the rest of the program to do anything in parallel.

   This has one possible conlusion, a user should never explicitly spawn their own
   domains.
*)
open Eio.Std

module Http = struct

  type Eio.Domain_manager.system += HTTP

  let fetch uri =
    let task () =
      (* Blocking work *)
      Unix.sleep 2;
      "Response for: " ^ Uri.to_string uri
    in
    Eio.Domain_manager.submit HTTP task
end

let main () =
  let uris = [
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
  ] in
  Fiber.List.iter (fun uri -> Http.fetch uri |> Eio.traceln "%s") uris

let () =
  Eio_main.run @@ fun _env ->
  main ()
