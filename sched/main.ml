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

module Http = struct
  module Par = struct
    type t = Eio.Domain_manager.parcap

    let init mgr = Eio.Domain_manager.register mgr ~name:"HTTP"

    let fetch mgr cap uri =
      let task () =
        Unix.sleep 2;
        "Response for: " ^ Uri.to_string uri
      in
      Eio.Domain_manager.submit mgr cap task
  end
end

let main mgr =
  let uris = [
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
    Uri.of_string "https://example.com";
  ] in
  let cap = Http.Par.init mgr in
  Eio.Fiber.List.iter (fun t -> Http.Par.fetch mgr cap t |> Eio.traceln "%s") uris

let () =
  Eio_main.run @@ fun env ->
  main env#domain_mgr

(* module Q = Eio_utils.Lf_queue

module Ark = struct
  type par_cap = int

  type 'a suspended = { k : ('a, [`Exit_scheduler]) Effect.Deep.continuation }

  type completed_task = E : 'a * 'a suspended -> completed_task

  type _ Effect.t += Yield : unit Effect.t

  let yield () = Effect.perform Yield

  type t = { 
    alloc : (par_cap, Domainslib.Task.pool) Hashtbl.t;
    queue : completed_task Q.t;
    mutable outstanding : int;
  }

  let s = { alloc = Hashtbl.create 128; queue = Q.create (); outstanding = 0  } 

  let register ~name =
    let id = ref 0 in
    fun v -> 
    let id = incr id; !id in
    Hashtbl.add v.alloc id (Domainslib.Task.setup_pool ~name ~num_domains:8 ());
    id

  let rec schedule () : [`Exit_scheduler] =
      match Q.pop s.queue with
      | Some (E (v, t)) -> 
        s.outstanding <- s.outstanding - 1;
        Effect.Deep.continue t.k v
      | None -> 
        if Q.is_empty s.queue && s.outstanding <= 0 then begin
          `Exit_scheduler
        end else schedule ()
  
  and submit cap task k =
    let pool = Hashtbl.find s.alloc cap in
    (* Slightly abusing the API for hacking purposes! *)
    s.outstanding <- s.outstanding + 1;
    let _ = Domainslib.Task.async pool (fun () ->
      Eio.traceln "Hello %i" (Domain.self () :> int);
      Q.push s.queue (E (task (), k))
    ) in
    Eio.traceln "Back to scheduler";
    schedule ()

  let run f =
    let open Effect.Deep in
    let fork f =
      match_with f s {
        retc = schedule;
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) -> match e with
          | Yield -> Some (fun (k : (a, _) continuation) ->
            let k = { k } in
            Q.push s.queue (E ((), k));
            schedule ()
          )
          | _ -> None
      }
    in
    let result = ref None in
    let `Exit_scheduler =
      fork (fun t -> 
        result := Some (f t)
      )
    in
    Option.get !result

end

module type Op = sig
  type t

  type value

  val pp : t Fmt.t

  val handle : t -> value
end
module Par (Op : Op) = struct
  type _ Effect.t += Par : Op.t -> Op.value Effect.t
  (* type _ Effect.t += Seq : (('a -> 'b) * 'a) -> 'b Effect.t *)
  (* let pipe f v = Effect.perform (Seq (f, v)) *)

  let task op = Effect.perform (Par op)

  let run : type a. ark:Ark.t -> (unit -> a) -> a = fun ~ark fn ->
    let open Effect.Deep in
    let par_cap = Ark.register ~name:"HTTP" ark in
      let fork fn =
      match_with fn () {
        retc = (fun v -> Ark.yield (); v);
        exnc = raise;
        effc = fun (type a) (e : a Effect.t) -> match e with
        | Par op -> Some (fun (k : (a, _) continuation) ->
          let k = { Ark.k } in 
          Eio.traceln "Handling %a" Op.pp op;
          let v = Ark.submit par_cap (fun () -> Op.handle op) k in
          Eio.traceln "submitted";
          v
        )
        | _ -> None
      }
    in
    let res = ref None in
    let `Exit_scheduler = 
      fork (fun () -> 
        let v = fn () in
        res := Some v;
        `Exit_scheduler)
    in 
    Option.get !res
end

module Http : sig
  type response = string
  val get : Uri.t -> response
  val run : ark:Ark.t -> (unit -> 'a) -> 'a
end = struct
  type response = string
  module Op = struct
    type t = Get of Uri.t

    type value = response

    let pp ppf = function
      | Get u -> Fmt.pf ppf "GET:%a" Uri.pp u 

    let handle = function
      | Get u ->
        (* Simulate work *)
        Unix.sleepf 2.;
        Fmt.str "[GOT]: %a" Uri.pp u
  end

  include Par(Op)

  let get uri = task (Get uri)
end

let main () =
  let uris = [ 
    Uri.of_string "http://tarides.com"; 
    Uri.of_string "http://github.com";
    Uri.of_string "https://kcsrk.info";
    Uri.of_string "https://anil.recoil.org";
    Uri.of_string "https://patrick.sirref.org";
  ] in
  let process uri = Http.(get uri |> String.length |> Eio.traceln "len: %i") in
  Eio.Fiber.List.iter process uris

let () =
  Ark.run @@ fun ark ->
  Eio_main.run @@ fun _env ->
  Http.run ~ark @@ fun () ->
  main ()

(* open Effect.Deep

type 'b next = E : ('a, 'b) continuation -> 'b next

type _ Effect.t += F : ('a -> 'b) -> 'b next Effect.t

type _ Effect.t += Pipe : ('a * 'b next) -> 'b Effect.t

let func f = Effect.perform (F f)
let ( >> ) v f = Effect.perform (Pipe (v, func f))

let add_one a = a + 1
let min_two a = a - 2

let prog () = 
  func add_one >> min_two

let () =
  let open Effect.Deep in
  print_int @@ 
  try_with prog () {
    effc = fun (type a) (e : a Effect.t) -> match e with
    | F f -> Some (fun k -> 
      
    )
    | Pipe (v, E k) -> Some (fun k -> 
      
    )
    | _ -> None
  } *) *)
