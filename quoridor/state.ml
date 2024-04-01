(* This file defines the representation of the game's state used in the engine. *)

(** Exception raised when a player attempts to place a wall without any remaining walls available. *)
exception Out_of_walls

(** Exception raised when placing a wall obstructs a player's path to victory. *)
exception Game_is_blocked

(** Exception raised when a player attempts an invalid or illegal move. *)
exception Illegal_move of string

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
let make ~rows ~columns ~wall_count ~wall_length ~to_play =
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

(** [split_diag_dir d] splits a diagonal direction into its components.
    Example:
    - [split_diag_dir 'NW'] returns ('N', 'W') 
    - [split_diag_dir 'N'] raise an assertion error. *)
let split_diag_dir (d : direction) : direction * direction =
  match d with
  | N | W | S | E -> assert false
  | NW -> (N, W)
  | SW -> (S, W)
  | SE -> (S, E)
  | NE -> (N, E)

(** Return the active player. *)
let active_player (game : t) : player_data =
  match game.to_play with PlayerA -> game.player_A | PlayerB -> game.player_B

(** Return the inactive player. *)
let inactive_player (game : t) : player_data =
  match game.to_play with PlayerA -> game.player_B | PlayerB -> game.player_A

(** The position of the active player. *)
let pos_active (game : t) : Board.pos = (active_player game).pawn_pos

(** The position of the inactive player*)
let pos_inactive (game : t) : Board.pos = (inactive_player game).pawn_pos

(** Compute the target position from the active player current position and a list of directions *)
let target_pos (game : t) (d : direction list) : Board.pos =
  List.fold_left Board.add_pos (pos_active game) (List.map pos_from_dir d)

(** Check that the position the active player wants to reach is not occupied. Raised an error if the position does not exist in the board *)
let is_free (game : t) (d : direction list) : bool =
  let p' = target_pos game d in
  if Board.pos_in_board game.board p'
  then begin
    p' <> pos_active game && p' <> pos_inactive game
  end
  else raise (Illegal_move "The target position does not exist in the board")

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
    Board.reachable game.board game.player_A.pawn_pos (win_pos_A game)
  in
  let able_to_win_B =
    Board.reachable game.board game.player_B.pawn_pos (win_pos_B game)
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
  if m <= 0 then raise Out_of_walls;
  match game.to_play with
  | PlayerA -> game.player_A.remaining_walls <- m - 1
  | PlayerB -> game.player_B.remaining_walls <- m - 1

(** Check if moving the active player's pawn in the direction d is valid,
    and if yes return the new position of the pawn. *)
let action_move_valid (game : t) (d : direction) : Board.pos =
  match d with
  | (N | E | S | W) as d ->
      if is_free game [ d ] && can_pass game [ d ]
      then target_pos game [ d ]
      else if (* Maybe he other player is next to us: maybe we can jump over him *)
              (not (is_free game [ d ]))
              && is_free game [ d; d ]
              && can_pass game [ d; d ]
      then target_pos game [ d; d ]
      else raise @@ Illegal_move "A wall obstructs the path"
  | (NE | SE | SW | NW) as d -> begin
      if not (is_free game [ d ])
      then raise @@ Illegal_move "Another player occupies that spot"
      else
        let d1, d2 = split_diag_dir d in
        let is_valid (d1 : direction) (d2 : direction) : bool =
          (* This function tests whether performing d1 then d2 is valid. *)
          (not (is_free game [ d1 ])) (* Inactive player at d1 from us *)
          && can_pass game [ d1; d2 ] (* No wall preventing to perform d1 d2 *)
          && not (can_pass game [ d1; d1 ])
          (* A wall preventing us to jump directly over the inactive player *)
        in
        if is_valid d1 d2 || is_valid d2 d1
        then target_pos game [ d ]
        else raise @@ Illegal_move "TODO"
    end

(** Check if the active player can place this wall. *)
let action_wall_valid (game : t) (w : Board.wall) : bool =
  try
    if remaining_walls game <= 0 then raise Out_of_walls;
    Board.add_wall game.board w;
    let game_is_blocked = is_blocked game in
    Board.remove_wall game.board w;
    not game_is_blocked
  with _ -> false

(** Execute a given (valid) action. This modifies the state in place. *)
let execute_action (game : t) (act : action) : unit =
  match act with
  | MovePawn d -> (active_player game).pawn_pos <- action_move_valid game d
  | PlaceWall w ->
      if remaining_walls game <= 0 then raise Out_of_walls;
      Board.add_wall game.board w;
      let game_is_blocked = is_blocked game in
      if game_is_blocked
      then (
        Board.remove_wall game.board w;
        raise Game_is_blocked)
      else decrement_walls game

(** Generate the list of all VALID actions. *)
let valid_actions (game : t) : action list =
  let directions = [ N; NW; W; SW; S; SE; E; NE ] in
  let move_pawn =
    List.map
      (fun d -> MovePawn d)
      (List.filter
         (fun d ->
           try
             ignore (action_move_valid game d);
             true
           with _ -> false)
         directions)
  in
  let walls = Board.generate_walls game.board game.wall_length in
  let place_wall =
    List.map (fun w -> PlaceWall w) (List.filter (action_wall_valid game) walls)
  in
  List.append move_pawn place_wall
