(* This file defines the representation of the game's state used in the engine. *)

(** Exception raised when placing a wall obstructs a player's path to victory. *)
exception BlockedPath

(** Exception raised when a player attempts to place a wall without any remaining walls available. *)
exception OutOfWalls

(** A tag identifying the two players.
    Player A starts on top of the board, and player B start at the bottom of the board. *)
type player = PlayerA | PlayerB

(** The directions a pawn can move in. *)
type direction = N | NW | W | SW | S | SE | E | NE

(** An action a player can take during their turn. *)
type action =
  | MovePawn of direction  (** Move the pawn of the current player. *)
  | PlaceWall of Board.wall  (** Place a wall for the current player. *)

(** The game state specific to a player. *)
type player_data =
  { mutable pawn_pos : Board.pos; mutable remaining_walls : int }

type t =
  { board : Board.t
  ; wall_length : int
  ; player_A : player_data
  ; player_B : player_data
  ; mutable to_play : player
  }

(** The state of the game.
    The board always has an odd number of columns. *)

(** Turn PlayerA to PlayerB and vice-versa. *)
let swap_player (p : player) : player =
  match p with PlayerA -> PlayerB | PlayerB -> PlayerA

(** Create a new game. The number of columns has to be odd.
    Player A starts in the middle of the top row (0), and player B starts in the middle of the bottom row. *)
let make rows columns wall_count wall_length to_play =
  if rows < 0
  then raise (Invalid_argument "rows")
  else if columns < 0 || columns mod 2 = 0
  then raise (Invalid_argument "columns")
  else if wall_count < 0
  then raise (Invalid_argument "wall_count")
  else
    let player_A =
      { pawn_pos = (0, columns / 2); remaining_walls = wall_count }
    in
    let player_B =
      { pawn_pos = (rows - 1, columns / 2); remaining_walls = wall_count }
    in
    let board = Board.make ~rows ~columns in
    { player_A; player_B; board; wall_length; to_play }

(** Position from a direction *)
let pos_from_dir (d : direction) : Board.pos =
  match d with
  | N -> (-1, 0)
  | NW -> (-1, -1)
  | W -> (0, -1)
  | SW -> (1, -1)
  | S -> (1, 0)
  | SE -> (1, 1)
  | E -> (0, 1)
  | NE -> (-1, 1)

(** Return the active player. *)
let active_player (game : t) : player_data =
  match game.to_play with PlayerA -> game.player_A | PlayerB -> game.player_B

(** Return the inactive player. *)
let inactive_player (game : t) : player_data =
  match game.to_play with PlayerA -> game.player_B | PlayerB -> game.player_A

(** The position of player A. *)
let pos_A (game : t) : Board.pos = game.player_A.pawn_pos

(** The position of player B. *)
let pos_B (game : t) : Board.pos = game.player_B.pawn_pos

(** The position of the active player. *)
let pos_active (game : t) : Board.pos = (active_player game).pawn_pos

(** The position of the inactive player*)
let pos_inactive (game : t) : Board.pos = (inactive_player game).pawn_pos

(** Compute the target position from the active player current position and a list of directions *)
let target_pos (game : t) (d : direction list) : Board.pos =
  List.fold_left Board.add_pos (pos_active game) (List.map pos_from_dir d)

(** Check that the position the active player wants to reach is not occupied and exist in the game *)
let is_free (game : t) (d : direction list) : bool =
  let p' = target_pos game d in
  p' <> pos_active game
  && p' <> pos_inactive game
  && Board.pos_in_board game.board p'

(** Check that there is not wall preventing the active player to reach where he wants to go*)
let can_pass (game : t) (dirs : direction list) : bool =
  List.for_all
    begin
      fun d ->
        match d with
        | (N | S | E | W) as d ->
            let p = pos_active game in
            let offset = pos_from_dir d in
            let p' = Board.add_pos p offset in
            Board.exist_edge game.board p p'
        | NW | SW | NE | SE -> false
    end
    dirs

(** Winning positions of player A (i.e. the bottom row). *)
let win_pos_A (game : t) (p : Board.pos) : bool =
  let rows = Board.rows game.board in
  match p with n, _ when n = rows - 1 -> true | _, _ -> false

(** Winning positions of player B (i.e. the top row). *)
let win_pos_B (_game : t) (p : Board.pos) : bool =
  match p with 0, _ -> true | _ -> false

(** Winning positions of active player. *)
let win_pos (game : t) : Board.pos -> bool =
  match game.to_play with
  | PlayerA -> win_pos_A game
  | PlayerB -> win_pos_B game

(** Check whether the active player wins. *)
let win (game : t) : bool = win_pos game (pos_active game)

(** A game is blocked if player A or player B are not able to win. *)
let is_blocked (game : t) : bool =
  let able_to_win_A =
    Board.reachable game.board (pos_A game) (win_pos_A game)
  in
  let able_to_win_B =
    Board.reachable game.board (pos_B game) (win_pos_B game)
  in
  not (able_to_win_A && able_to_win_B)

(** Remaining walls of the active player. *)
let remaining_walls (game : t) : int =
  match game.to_play with
  | PlayerA -> game.player_A.remaining_walls
  | PlayerB -> game.player_B.remaining_walls

(** Decrement the number of remaining walls of the active player*)
let decrement_walls (game : t) : unit =
  let m = remaining_walls game in
  if m <= 0 then raise OutOfWalls;
  match game.to_play with
  | PlayerA -> game.player_A.remaining_walls <- m - 1
  | PlayerB -> game.player_B.remaining_walls <- m - 1

(** Check if moving the active player's pawn in the direction d is valid,
    and if yes return the new position of the pawn. *)
let move_pawn_valid (game : t) (d : direction) : Board.pos option =
  match d with
  | (N | E | S | W) as d ->
      if (* Check if the position located at direction d from the active player is both in the board and free *)
         is_free game [ d ]
      then
        if (* Check there is no wall preventing the active player to move *)
           can_pass game [ d ]
        then Some (target_pos game [ d ])
        else None
      else if (* Maybe the other player is next to us and we can jump over him *)
              is_free game [ d; d ] && can_pass game [ d; d ]
      then Some (target_pos game [ d; d ])
      else None
  | (NE | SE | SW | NW) as d -> begin
      if not (is_free game [ d ])
      then None
      else
        match d with
        | NW ->
            if (* Either the other player is at direction N or W of the active player *)
               not (is_free game [ N ])
            then
              (* Inactive player is at the N of active player*)
              if can_pass game [ N; W ]
                 (* Active player can go at N then W *)
                 && not (can_pass game [ N; N ])
                 (* There is a wall preventing the active player to jump directly over the inactive player *)
              then Some (target_pos game [ N; W ])
              else None
            else if not (is_free game [ W ])
            then
              (* Inactive player is at the W of active player *)
              if can_pass game [ W; N ] && not (can_pass game [ W; W ])
              then Some (target_pos game [ N; W ])
              else None
            else None
        | SW ->
            if not (is_free game [ S ])
            then
              if can_pass game [ S; W ] && not (can_pass game [ S; S ])
              then Some (target_pos game [ S; W ])
              else None
            else if not (is_free game [ W ])
            then
              if can_pass game [ W; S ] && not (can_pass game [ W; W ])
              then Some (target_pos game [ S; W ])
              else None
            else None
        | SE ->
            if not (is_free game [ S ])
            then
              if can_pass game [ S; E ] && not (can_pass game [ S; S ])
              then Some (target_pos game [ S; E ])
              else None
            else if not (is_free game [ E ])
            then
              if can_pass game [ E; S ] && not (can_pass game [ E; E ])
              then Some (target_pos game [ S; E ])
              else None
            else None
        | NE ->
            if not (is_free game [ N ])
            then
              if can_pass game [ N; E ] && not (can_pass game [ N; N ])
              then Some (target_pos game [ N; E ])
              else None
            else if not (is_free game [ E ])
            then
              if can_pass game [ E; N ] && not (can_pass game [ E; E ])
              then Some (target_pos game [ N; E ])
              else None
            else None
        | _ -> assert false
    end

(** Check if the active player can place this wall. *)
let place_wall_valid (game : t) (w : Board.wall) : bool =
  try
    if remaining_walls game <= 0 then raise OutOfWalls;
    Board.add_wall game.board w;
    let game_is_blocked = is_blocked game in
    Board.remove_wall game.board w;
    not game_is_blocked
  with _ -> false

(** Execute a given (valid) action. This modifies the state in place. *)
let execute_action (game : t) (act : action) : bool =
  match act with
  | MovePawn d -> (
      match move_pawn_valid game d with
      | Some p ->
          (active_player game).pawn_pos <- p;
          true
      | None -> false)
  | PlaceWall w -> (
      try
        if remaining_walls game <= 0 then raise OutOfWalls;
        Board.add_wall game.board w;
        let game_is_blocked = is_blocked game in
        if game_is_blocked
        then (
          Board.remove_wall game.board w;
          false)
        else (
          decrement_walls game;
          true)
      with _ -> false)

(** Switch the player turn. *)
let switch_player (game : t) : unit =
  game.to_play <-
    (match game.to_play with PlayerA -> PlayerB | PlayerB -> PlayerA)

(** Generate the list of all VALID actions. *)
let generate_actions (game : t) : action list =
  let directions = [ N; NW; W; SW; S; SE; E; NE ] in
  let move_pawn =
    List.map
      (fun d -> MovePawn d)
      (List.filter
         (fun d ->
           match move_pawn_valid game d with None -> false | Some _ -> true)
         directions)
  in
  let walls = Board.generate_walls game.board game.wall_length in
  let place_wall =
    List.map (fun w -> PlaceWall w) (List.filter (place_wall_valid game) walls)
  in
  List.append move_pawn place_wall
