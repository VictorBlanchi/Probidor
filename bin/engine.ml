open Quoridor
open Sockets
open Lwt.Syntax

(** All the data the engine needs to keep track of in a game. *)
type game =
  { connA : Server.connection  (** Connection to player A. *)
  ; connB : Server.connection  (** Connection to player B. *)
  ; state : State.t  (** Game state. *)
  }

(** Return the connections to the active player and inactive player. *)
let active_inactive_conns (game : game) : Server.connection * Server.connection =
  match game.state.to_play with
  | PlayerA -> (game.connA, game.connB)
  | PlayerB -> (game.connB, game.connA)

(** Create a new game. *)
let create_game () : game Lwt.t =
  (* Connect to the two players. *)
  let* conns = Server.connect_to_clients 2 in
  let connA, connB =
    match conns with
    | [ connA; connB ] -> (connA, connB)
    | _ -> raise (Failure "Server.connect_to_clients returned an invalid number of connections.")
  in
  (* Create the game state. *)
  let state = State.make ~rows:9 ~columns:9 ~wall_count:10 ~wall_length:2 ~to_play:PlayerA in
  Lwt.return { connA; connB; state }

let greet_players (_game : game) : unit Lwt.t = failwith "todo"

(* Play a game. We always listen for a message from the active player. *)
let rec play (game : game) : unit Lwt.t =
  let act_conn, inact_conn = active_inactive_conns game in
  (* Receive a request from the active player. *)
  let* req = Server.receive_request act_conn in
  match req with
  | NewPlayer ->
      (* The game is already initialized : this is an invalid request. *)
      let* () =
        Server.send_response act_conn
        @@ Protocol.Error "Invalid request NewPlayer (the game has already started)."
      in
      play game
  | DoAction action -> begin
      try
        (* Execute the active player's action. *)
        State.execute_action game.state action;
        (* Check if the active player won. *)
        if State.win game.state
        then
          (* The active player won ! Notify both players. *)
          let* () = Server.send_response act_conn YouWin in
          let* () = Server.send_response inact_conn @@ OppAction { action; win = true } in
          (* The game is finished. *)
          Lwt.return ()
        else
          (* Send the action to the inactive player. *)
          let* () = Server.send_response inact_conn @@ OppAction { action; win = false } in
          play game
      with _ ->
        (* The action was invalid : send an error message to the current player. *)
        let* () = Server.send_response act_conn @@ Protocol.Error "invalid move" in
        play game
    end
  | ValidActions ->
      (* Send the list of valid actions to the active player. *)
      let actions = State.valid_actions game.state in
      let* () = Server.send_response act_conn @@ ActionList actions in
      play game

let () =
  Lwt_main.run
    begin
      let* game = create_game () in
      let* () = greet_players game in
      play game
    end
