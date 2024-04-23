open Quoridor
open Sockets
open OUnit2

(*************************************************************************************)
(* Test Monad *)

(** The state of our state monad. *)
type state =
  { requestsA : Protocol.request list
  ; requestsB : Protocol.request list
  ; responsesA : Protocol.response list
  ; responsesB : Protocol.response list
  }

(** We use a simple state monad. 
    Responses are collected and stored in the state. 
    Requests are predefined and drawn (in order) from the state. *)
type 'a m = state -> 'a * state

module M : Engine.Monad with type 'a t = 'a m = struct
  type 'a t = 'a m

  let return x state = (x, state)

  let bind m f state =
    let x, state = m state in
    f x state
end

(** Run a computation in our custom monad. *)
let run (m : 'a M.t) state : 'a * state = m state

(** Exception raised when we exhausted the list of predefined requests for a player. *)
exception ExhaustedRequests of State.player

(** A dummy logger that ignores all messages. *)
module L : Engine.Logger with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t
  type level = Info | Debug | Error

  let log _ fmt = Format.ksprintf (fun _ -> M.return ()) fmt
end

module C : Engine.Connection with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t

  let receive_request player state =
    match player with
    | State.PlayerA -> begin
        match state.requestsA with
        | [] -> raise @@ ExhaustedRequests State.PlayerA
        | req :: reqs -> (req, { state with requestsA = reqs })
      end
    | State.PlayerB -> begin
        match state.requestsB with
        | [] -> raise @@ ExhaustedRequests State.PlayerB
        | req :: reqs -> (req, { state with requestsB = reqs })
      end

  let send_response player resp state =
    match player with
    | State.PlayerA -> ((), { state with responsesA = resp :: state.responsesA })
    | State.PlayerB -> ((), { state with responsesB = resp :: state.responsesB })
end

(* Instantiate the engine. *)
module E = Engine.Make (M) (L) (C)

(*************************************************************************************)
(* Unit Tests *)
let tests =
  "test suite for sum"
  >::: [ ("empty" >:: fun _ -> assert_equal 0 (sum []))
       ; ("singleton" >:: fun _ -> assert_equal 1 (sum [ 1 ]))
       ; ("two_elements" >:: fun _ -> assert_equal 3 (sum [ 1; 2 ]))
       ]
