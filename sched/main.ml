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

type response = string
(** Response to a get request (a string for simplicity) *)

type _ Effect.t += Get : Uri.t -> string Effect.t

(* Attempt 2

   Effects based map-reduce... or at least the map part.
*)

module Par = struct
  open Effect

  type _ Effect.t += Start : 'a Hmap.key Effect.t
  type _ Effect.t += Task : ((unit -> 'b) list Hmap.key * (unit -> 'b)) -> unit Effect.t
  type _ Effect.t += End : (unit -> 'b) list Hmap.key -> 'b list Effect.t

  let map tasks =
    let id = perform Start in
    List.iter (fun task -> perform (Task (id, task))) tasks;
    perform (End id)
end

module Http = struct
  open Effect.Deep


  let run _mgr fn =
    (* Should actually be domain-safe! *)
    let groups = ref Hmap.empty in
    let fork fn =
      try_with fn () {
        effc = fun (type a) (e : a Effect.t) -> match e with
        | Par.Start -> 
          let v = Hmap.Key.create () in
          Some (fun (k : ((a, _) continuation)) -> continue k v)
        | Par.Task (key, task) -> Some (fun k -> 
          let new_map = match Hmap.find key !groups with
            | Some tasks -> Hmap.add key (task :: tasks) !groups;
            | None -> Hmap.add key [ task ] !groups
          in
          groups := new_map;
          continue k ()
        )
        | Par.End key -> Some (fun k ->
          (* Dumb parallel implementation *)
          let tasks = Hmap.get key !groups |> List.rev in
          (* Ideally we would reinstall the HTTP handler in case the callback does some more HTTP things!  
             let doms = List.map (fun t -> Domain.spawn (fun () -> fork t)) tasks in
             
             The extra fork is really important otherwise it basically runs sequentially
             because of how domain manager works!

             I think more to be learnt from https://watch.ocaml.org/videos/watch/08ea09a1-e645-47cb-80c4-499dd4d93ac8
             *)
          let fin = 
            Eio.Fiber.List.map (Eio.Domain_manager.run _mgr) tasks
          in
          continue k fin
        )
        | _ -> None
      }
    in
      fork fn
end

let main () =
  let fetch i uri () =
    (* Actually block the fiber to simulate computational work *)
    Unix.sleepf 2.;
    "req: " ^ Uri.to_string uri ^ "\nres: " ^ string_of_int i
  in
  let tasks = List.mapi fetch [ 
    Uri.of_string "http://tarides.com"; 
    Uri.of_string "http://github.com";
    Uri.of_string "https://kcsrk.info";
    Uri.of_string "https://anil.recoil.org";
    Uri.of_string "https://patrick.sirref.org";
  ] in
  let resp = Par.map tasks in
  List.iter (Eio.traceln "%s") resp

let () =
  Eio_main.run @@ fun env ->
  Http.run env#domain_mgr @@ fun () ->
  main ()
