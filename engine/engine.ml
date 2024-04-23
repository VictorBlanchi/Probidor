open Quoridor
open Sockets

(** The game engine executes in a monad. 
    Typically this will be Lwt.t, but for testing we use a different monad. *)
module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'b t) -> 'b t
end

(** The game engine abstracts over the type of connection used to 
    communicated to players. This makes testing easier. *)
module type Connection = sig
  type 'a t

  val receive_request : State.player -> Protocol.request t
  val send_response : State.player -> Protocol.response -> unit t
end

(** The game engine is parameterized over a logger. *)
module type Logger = sig
  type 'a t
  type level = Info | Debug | Error

  val log : level -> ('a, unit, string, unit t) format4 -> 'a
end

module Make
    (M : Monad)
    (L : Logger with type 'a t = 'a M.t)
    (C : Connection with type 'a t = 'a M.t) : sig
  val play : unit -> unit M.t
end = struct
  let ( let* ) = M.bind

  (** The players are asigned player A and player B arbitrarily. Player A starts. 
      We wait for a NewPlayer message from each player, and send them the game info. *)
  let greet_players (game : State.t) : unit M.t =
    (* Wait for a player to send a [NewPlayer] request. *)
    let rec handle_new_player player =
      let* req = C.receive_request player in
      match req with
      | Protocol.NewPlayer -> L.log Info "Received [NewPlayer] from %s" (State.show_player player)
      | _ ->
          (* Invalid request : send an error response and wait for the player to try again. *)
          let* () =
            L.log Error "Received invalid request from %s: expected [NewPlayer]. Trying again..."
              (State.show_player player)
          in
          let* () =
            C.send_response player @@ Protocol.Error "Invalid request: expected [NewPlayer]."
          in
          handle_new_player player
    in
    (* Handle both players. *)
    let* () = handle_new_player PlayerA in
    let* () = handle_new_player PlayerB in
    (* Once both players have sent a request, send a Welcome response to them. *)
    let rows = Board.rows game.board in
    let cols = Board.columns game.board in
    let wall_count = game.player_A.remaining_walls in
    let wall_length = game.wall_length in
    (* Response to Player A. *)
    let* () =
      C.send_response State.PlayerA
      @@ Welcome
           { rows
           ; cols
           ; wall_count
           ; wall_length
           ; your_pawn = game.player_A.pawn_pos
           ; opp_pawn = game.player_B.pawn_pos
           ; you_start = game.to_play = PlayerA
           }
    in
    (* Response to Player B. *)
    let* () =
      C.send_response State.PlayerB
      @@ Welcome
           { rows
           ; cols
           ; wall_count
           ; wall_length
           ; your_pawn = game.player_B.pawn_pos
           ; opp_pawn = game.player_A.pawn_pos
           ; you_start = game.to_play = PlayerB
           }
    in
    let* () = L.log Info "Sent [Welcome] to both players." in
    M.return ()

  (** The main game loop. We always listen for a message from the active player. *)
  let rec loop (game : State.t) : unit L.t =
    let* () = L.log Info "%s to play." (State.show_player game.to_play) in
    (* Receive a request from the active player. *)
    let* req = C.receive_request game.to_play in
    let* () =
      L.log Info "Received request from player %s: %s" (State.show_player game.to_play)
        (Yojson.Basic.pretty_to_string @@ Protocol.encode_request req)
    in
    match req with
    | NewPlayer ->
        (* The game is already initialized : this is an invalid request. *)
        let* () = L.log Error "Invalid request. Trying again..." in
        let* () =
          C.send_response game.to_play
          @@ Protocol.Error "Invalid request NewPlayer (the game has already started)."
        in
        loop game
    | DoAction action -> begin
        try
          (* Execute the active player's action. *)
          State.execute_action game action;
          (* Check if the active player won. *)
          if State.win game
          then
            (* The active player won ! Notify both players. *)
            let* () = L.log Info "Player %s won !" (State.show_player game.to_play) in
            let* () = C.send_response game.to_play YouWin in
            let* () =
              C.send_response (State.swap_player game.to_play) @@ OppAction { action; win = true }
            in
            (* The game is finished. *)
            M.return ()
          else
            (* Send the action to the inactive player. *)
            let* () =
              C.send_response (State.swap_player game.to_play) @@ OppAction { action; win = false }
            in
            loop game
        with (State.IllegalMove _ | State.IllegalWall _) as err ->
          (* Get the reason why the move was illegal, as a string. *)
          let reason =
            match err with
            | State.IllegalMove reason -> State.show_illegal_move reason
            | State.IllegalWall reason -> State.show_illegal_wall reason
            | _ -> failwith "Engine.play"
          in
          (* Send an error message to the current player. *)
          let* () = L.log Error "Illegal move: %s. Trying again..." reason in
          let* () =
            C.send_response game.to_play
            @@ Protocol.Error (Format.sprintf "Illegal move: %s." reason)
          in
          loop game
      end
    | ValidActions ->
        (* Send the list of valid actions to the active player. *)
        let actions = State.valid_actions game in
        let* () = C.send_response game.to_play @@ ActionList actions in
        loop game

  let play () : unit M.t =
    let game = State.make ~rows:9 ~columns:9 ~wall_count:10 ~wall_length:2 ~to_play:PlayerA in

    let* () = greet_players game in
    loop game
end
