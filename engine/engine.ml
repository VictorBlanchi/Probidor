open Quoridor
open Lwt.Syntax
open Sockets

module type Connection = sig
  type t

  val connect : unit -> (t * t) Lwt.t
  val send_response : t -> Protocol.response -> unit Lwt.t
  val receive_request : t -> Protocol.request Lwt.t
end

module type Logger = sig
  val log : ('a, unit, string, unit Lwt.t) format4 -> 'a
end

module type Sig = sig
  val play : unit -> unit Lwt.t
end

module Make (L : Logger) (C : Connection) = struct
  (** All the data the engine needs to keep track of in a game. *)
  type game =
    { connA : C.t  (** Connection to player A. *)
    ; connB : C.t  (** Connection to player B. *)
    ; state : State.t  (** Game state. *)
    }

  (** Return the connection corresponding to a player. *)
  let player_conn (game : game) (player : State.player) : C.t =
    match player with State.PlayerA -> game.connA | State.PlayerB -> game.connB

  (** Return the connections to the active player and inactive player. *)
  let active_inactive_conns (game : game) : C.t * C.t =
    match game.state.to_play with
    | PlayerA -> (game.connA, game.connB)
    | PlayerB -> (game.connB, game.connA)

  (** Create a new game and wait for players to connect. 
    The players are asigned player A and player B arbitrarily.
    Player A starts. *)
  let create_game () : game Lwt.t =
    (* Connect to the two players. *)
    let addr = Unix.inet6_addr_loopback in
    let port = 8000 in
    let* () =
      L.log "Listening for player connections on %s/%d." (Unix.string_of_inet_addr addr) port
    in
    let* connA, connB = C.connect () in
    let* () = L.log "Connected to players." in
    (* Create the game state. *)
    let state = State.make ~rows:9 ~columns:9 ~wall_count:10 ~wall_length:2 ~to_play:PlayerA in
    Lwt.return { connA; connB; state }

  (** Wait for a NewPlayer message from each player, and send them *)
  let greet_players (game : game) : unit Lwt.t =
    (* Wait for a player to send a [NewPlayer] request. *)
    let rec handle_new_player player =
      let* req = C.receive_request (player_conn game player) in
      match req with
      | Protocol.NewPlayer ->
          let* () = L.log "Received [NewPlayer] from %s" (State.show_player player) in
          Lwt.return ()
      | _ ->
          (* Invalid request : send an error response and wait for the player to try again. *)
          let* () =
            L.log "Received invalid request from %s: expected [NewPlayer]. Trying again..."
              (State.show_player player)
          in
          let* () =
            C.send_response (player_conn game player)
            @@ Protocol.Error "Invalid request: expected [NewPlayer]."
          in
          handle_new_player player
    in
    (* Handle both players simultaneously. *)
    let* _ = Lwt.both (handle_new_player PlayerA) (handle_new_player PlayerB) in
    (* Once both players have sent a request, send a Welcome response to them. *)
    let rows = Board.rows game.state.board in
    let cols = Board.columns game.state.board in
    let wall_count = game.state.player_A.remaining_walls in
    let wall_length = game.state.wall_length in
    (* Response to Player A. *)
    let* () =
      C.send_response game.connA
      @@ Welcome
           { rows
           ; cols
           ; wall_count
           ; wall_length
           ; your_pawn = game.state.player_A.pawn_pos
           ; opp_pawn = game.state.player_B.pawn_pos
           ; you_start = game.state.to_play = PlayerA
           }
    in
    (* Response to Player B. *)
    let* () =
      C.send_response game.connB
      @@ Welcome
           { rows
           ; cols
           ; wall_count
           ; wall_length
           ; your_pawn = game.state.player_B.pawn_pos
           ; opp_pawn = game.state.player_A.pawn_pos
           ; you_start = game.state.to_play = PlayerB
           }
    in
    L.log "Sent [Welcome] to both players."

  (** Run the main game loop. We always listen for a message from the active player. *)
  let rec game_loop (game : game) : unit Lwt.t =
    let* () = L.log "%s to play." (State.show_player game.state.to_play) in
    let act_conn, inact_conn = active_inactive_conns game in
    (* Receive a request from the active player. *)
    let* req = C.receive_request act_conn in
    let* () =
      L.log "Received request from player %s: %s"
        (State.show_player game.state.to_play)
        (Yojson.Basic.pretty_to_string @@ Protocol.encode_request req)
    in
    match req with
    | NewPlayer ->
        (* The game is already initialized : this is an invalid request. *)
        let* () = L.log "Invalid request. Trying again..." in
        let* () =
          C.send_response act_conn
          @@ Protocol.Error "Invalid request NewPlayer (the game has already started)."
        in
        game_loop game
    | DoAction action -> begin
        try
          (* Execute the active player's action. *)
          State.execute_action game.state action;
          (* Check if the active player won. *)
          if State.win game.state
          then
            (* The active player won ! Notify both players. *)
            let* () = L.log "Player %s won !" (State.show_player game.state.to_play) in
            let* () = C.send_response act_conn YouWin in
            let* () = C.send_response inact_conn @@ OppAction { action; win = true } in
            (* The game is finished. *)
            Lwt.return ()
          else
            (* Send the action to the inactive player. *)
            let* () = C.send_response inact_conn @@ OppAction { action; win = false } in
            game_loop game
        with (State.IllegalMove _ | State.IllegalWall _) as err ->
          (* Get the reason why the move was illegal, as a string. *)
          let reason =
            match err with
            | State.IllegalMove reason -> State.show_illegal_move reason
            | State.IllegalWall reason -> State.show_illegal_wall reason
            | _ -> failwith "Engine.play"
          in
          (* Send an error message to the current player. *)
          let* () = L.log "Illegal move: %s. Trying again..." reason in
          let* () =
            C.send_response act_conn @@ Protocol.Error (Format.sprintf "Illegal move: %s." reason)
          in
          game_loop game
      end
    | ValidActions ->
        (* Send the list of valid actions to the active player. *)
        let actions = State.valid_actions game.state in
        let* () = C.send_response act_conn @@ ActionList actions in
        game_loop game

  let play () =
    let* game = create_game () in
    let* () = greet_players game in
    game_loop game
end
