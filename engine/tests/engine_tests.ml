open Quoridor
open Sockets
open OUnit2
open Protocol

(*************************************************************************************)
(* Test Monad *)

(** The state of our state monad. *)
type state =
  { requestsA : request list
  ; requestsB : request list
  ; responsesA : response list
  ; responsesB : response list
  }

(** We use a state monad and an option monad (to enable early exit).
    Responses are collected and stored in the state. 
    Requests are predefined and drawn (in order) from the state. *)
type 'a m = state -> 'a option * state

module M : Engine.Monad with type 'a t = 'a m = struct
  type 'a t = 'a m

  let return x state = (Some x, state)

  let bind m f state =
    match m state with None, state -> (None, state) | Some x, state -> f x state
end

(** Run a computation in our custom monad. *)
let run (m : 'a M.t) state : 'a option * state = m state

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

  (** This returns [None] if there are no more predefined requests for the player. *)
  let receive_request player state =
    match player with
    | State.PlayerA -> begin
        match state.requestsA with
        | [] -> (None, state)
        | req :: reqs -> (Some req, { state with requestsA = reqs })
      end
    | State.PlayerB -> begin
        match state.requestsB with
        | [] -> (None, state)
        | req :: reqs -> (Some req, { state with requestsB = reqs })
      end

  let send_response player resp state =
    match player with
    | State.PlayerA -> (Some (), { state with responsesA = resp :: state.responsesA })
    | State.PlayerB -> (Some (), { state with responsesB = resp :: state.responsesB })
end

(* Instantiate the engine. *)
module E = Engine.Make (M) (L) (C)

(*************************************************************************************)
(* Unit Tests *)

(** A single game. It is parameterized by the predefined requests from the players,
    and returns the responses sent by the engine. *)
let run_predefined requestsA requestsB =
  let in_state = { requestsA; requestsB; responsesA = []; responsesB = [] } in
  let _, out_state = run (E.play ()) in_state in
  (List.rev out_state.responsesA, List.rev out_state.responsesB)

(** [assert_prefix fs xs] checks that the functions in [fs] are true on a prefix of [xs],
    and raises an exception if it is not the case. *)
let rec assert_prefix fs xs =
  match fs with
  | [] -> ()
  | f :: fs -> begin
      match xs with
      | [] ->
          assert_failure "assert_prefix failed : second argument is shorter than first argument."
      | x :: xs ->
          assert_bool "assert_prefix" (f x);
          assert_prefix fs xs
    end

(** Is a response of the form [Welcome _] ?*)
let is_welcome resp = match resp with Welcome _ -> true | _ -> false

(** Is a response of the form [Error _] ?*)
let is_error resp = match resp with Error _ -> true | _ -> false

(** Is a response of the form [OppAction _] ? Optionnally, specify the exact action we expect. *)
let is_opp_action ?(action : State.action option) resp : bool =
  match resp with
  | OppAction opp -> begin match action with None -> true | Some action -> action = opp.action end
  | _ -> false

(** The constant predicate equal to [true]. *)
let true_ _ = true

let tests =
  let open State in
  [ (* Just authenticate players. *)
    "welcome-ok"
    >:: begin
          fun _ ->
            let respsA, respsB = run_predefined [ NewPlayer ] [ NewPlayer ] in
            assert_prefix [ is_welcome ] respsA;
            assert_prefix [ is_welcome ] respsB
        end
  ; (* Play a valid sequence of pawn moves. *)
    "move-pawn-valid"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer; DoAction (MovePawn S); DoAction (MovePawn E) ]
                [ NewPlayer; DoAction (MovePawn N); DoAction (MovePawn W) ]
            in
            assert_prefix
              [ is_welcome; is_opp_action ~action:(MovePawn N); is_opp_action ~action:(MovePawn W) ]
              respsA;
            assert_prefix
              [ is_welcome; is_opp_action ~action:(MovePawn S); is_opp_action ~action:(MovePawn E) ]
              respsB
        end
  ; (* Attempt to send several [NewPlayer] requests. *)
    "welcome-multiple-A"
    >:: begin
          fun _ ->
            let respsA, respsB = run_predefined [ NewPlayer; NewPlayer; NewPlayer ] [ NewPlayer ] in
            assert_prefix [ is_welcome; is_error; is_error ] respsA;
            assert_prefix [ is_welcome ] respsB
        end
  ; (* Attempt to send several [NewPlayer] requests, and continue playing. *)
    "welcome-multiple-B"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer; DoAction (MovePawn E) ]
                [ NewPlayer; NewPlayer; NewPlayer; DoAction (MovePawn N) ]
            in
            assert_prefix [ is_welcome; is_opp_action ~action:(MovePawn N) ] respsA;
            assert_prefix [ is_welcome; is_opp_action; is_error; is_error ] respsB
        end
  ; (* Attempt to move a pawn in an invalid direction. *)
    "move-pawn-invalid-A"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer; DoAction (MovePawn N); DoAction (MovePawn S) ]
                [ NewPlayer ]
            in
            assert_prefix [ is_welcome; is_error (* *) ] respsA;
            assert_prefix [ is_welcome; is_opp_action ~action:(MovePawn S) ] respsB
        end
  ; (* Attempt to move a pawn in an invalid direction. *)
    "move-pawn-invalid-B"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer; DoAction (MovePawn S) ]
                [ NewPlayer; DoAction (MovePawn NE); DoAction (MovePawn N) ]
            in
            assert_prefix [ is_welcome; is_opp_action ~action:(MovePawn N) ] respsA;
            assert_prefix [ is_welcome; is_opp_action ~action:(MovePawn S) ] respsB
        end
  ; (* Attempt to cross a wall. *)
    "place-wall-block-1"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer
                ; DoAction (PlaceWall Board.{ horizontal = true; length = 2; pos = (7, 3) })
                ]
                [ NewPlayer; DoAction (MovePawn N) ]
            in
            assert_prefix [ is_welcome ] respsA;
            assert_prefix [ is_welcome; is_opp_action; is_error ] respsB
        end
  ; (* Attempt to cross a wall. *)
    "place-wall-block-2"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer
                ; DoAction (PlaceWall Board.{ horizontal = true; length = 2; pos = (7, 4) })
                ]
                [ NewPlayer; DoAction (MovePawn N) ]
            in
            assert_prefix [ is_welcome ] respsA;
            assert_prefix [ is_welcome; is_opp_action; is_error ] respsB
        end
  ; (* Attempt to place a wall with invalid length. *)
    "place-wall-invalid-length"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer
                ; DoAction (PlaceWall Board.{ horizontal = true; length = 3; pos = (7, 3) })
                ]
                [ NewPlayer ]
            in
            assert_prefix [ is_welcome; is_error ] respsA;
            assert_prefix [ is_welcome ] respsB
        end
  ; (* Attempt to place a wall out of bounds. *)
    "place-wall-out-of-bounds"
    >:: begin
          fun _ ->
            let respsA, respsB =
              run_predefined
                [ NewPlayer
                ; DoAction (PlaceWall Board.{ horizontal = true; length = 2; pos = (7, 8) })
                ]
                [ NewPlayer ]
            in
            assert_prefix [ is_welcome; is_error ] respsA;
            assert_prefix [ is_welcome ] respsB
        end
  ]

let () = run_test_tt_main ("engine tests" >::: tests)
