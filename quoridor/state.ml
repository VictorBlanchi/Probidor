(* This file defines the representation of the game's state used in the engine. *)

exception BlockedPath
(** If placing a wall obstructs a player's path to victory, this exception is raised. *)

exception OutOfWall
(** Exception raised when a player attempts to place a wall without any remaining walls available. *)

(** Two distinguish between player. Player A starts on top of the board. *)
type player = PlayerA | PlayerB

(** The directions a pawn can move in. *)
type direction = N | NW | W | SW | S | SE | E | NE

type action =
  | MovePawn of direction
  | PlaceWall of Board.wall
      (** An action a player can take during their turn. *)

type player_data = { pawn_pos : Board.pos; remaining_walls : int }
(** The game state specific to a player. *)

type t = {
  board : Board.t;
  player_A : player_data;
  player_B : player_data;
  to_play : player;
}
(** The state of the game.
    The board always has an odd number of columns.  *)

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

(** The position of the pawn of player A. *)
let pawn_pos_A (game : t) : Board.pos = game.player_A.pawn_pos

(** The position of the pawn of player B. *)
let pawn_pos_B (game : t) : Board.pos = game.player_B.pawn_pos

(** The position of the active player*)
let pawn_pos_active (game : t) : Board.pos =
  match game.to_play with
  | PlayerA -> pawn_pos_A game
  | PlayerB -> pawn_pos_B game

(** The position of the inactive player*)
let pawn_pos_inactive (game : t) : Board.pos =
  match game.to_play with
  | PlayerA -> pawn_pos_B game
  | PlayerB -> pawn_pos_A game

(** Compute the target position from the active player current position and a list of directions *)
let target_pos (game : t) (d : direction list) : Board.pos =
  List.fold_left Board.add_pos (pawn_pos_active game) (List.map pos_from_dir d)

(** Check that the position the active player wants to reach is not occupied and exist in the game *)
let is_free (game : t) (d : direction list) : bool =
  let p' = target_pos game d in
  pawn_pos_inactive game = p' && Board.pos_in_board game.board p'

(** Check that there is not wall preventing the active player to reach where he wants to go*)
let can_pass (game : t) (d : direction list) : bool =
  let aux (d : direction) =
    match d with
    | (N | S | E | W) as d ->
        let p = pawn_pos_active game in
        let offset = pos_from_dir d in
        let p' = Board.add_pos p offset in
        Board.exist_edge game.board p p'
    | NW | SW | NE | SE -> false
  in
  List.fold_left ( && ) true (List.map aux d)

(** Winning positions of player A. *)
let win_pos_A (_game : t) (p : Board.pos) : bool =
  match p with 0, _ -> true | _, _ -> false

(** Winning positions of player B. *)
let win_pos_B (game : t) (p : Board.pos) : bool =
  let rows = Board.rows game.board in
  match p with n, _ when n = rows - 1 -> true | _ -> false

(** Winning positions of active player. *)
let win_pos_active (game : t) : Board.pos -> bool =
  match game.to_play with
  | PlayerA -> win_pos_A game
  | PlayerB -> win_pos_B game

(** Winning positions of inactive player. *)
let win_pos_inactive (game : t) : Board.pos -> bool =
  match game.to_play with
  | PlayerA -> win_pos_B game
  | PlayerB -> win_pos_A game

(** Create a new game. The number of columns has to be odd. 
    Player A starts in the middle of the top row (0), and player B starts in the middle of the bottom row. *)
let make rows columns wall_count to_play =
  if rows < 0 then raise (Invalid_argument "rows")
  else if columns < 0 || columns mod 2 = 0 then
    raise (Invalid_argument "columns")
  else if wall_count < 0 then raise (Invalid_argument "wall_count")
  else
    let player_A =
      { pawn_pos = (0, columns / 2); remaining_walls = wall_count }
    in
    let player_B =
      { pawn_pos = (rows - 1, columns / 2); remaining_walls = wall_count }
    in
    let board = Board.make ~rows ~columns in
    { player_A; player_B; board; to_play }

(** A game is blocked if player A or player B are not able to win *)
let is_blocked (game : t) : bool =
  let able_to_win_A =
    Board.reachable game.board (pawn_pos_A game) (win_pos_A game)
  in
  let able_to_win_B =
    Board.reachable game.board (pawn_pos_B game) (win_pos_B game)
  in
  not (able_to_win_A && able_to_win_B)

(** Check if moving the active player's pawn in the direction d is valid. *)
let move_pawm_valid (game : t) (d : direction) : Board.pos option =
  match d with
  | (N | E | S | W) as d ->
      if
        is_free game [ d ]
        (* Check if the position located at direction d from the active player is both in the board and free *)
      then
        if
          can_pass game [ d ]
          (* Check there is no wall preventing the active player to move *)
        then Some (target_pos game [ d ])
        else None
      else if
        is_free game [ d; d ] && can_pass game [ d; d ]
        (* Maybe the other player is next to us and we can jump over him *)
      then Some (target_pos game [ d; d ])
      else None
  | (NE | SE | SW | NW) as d -> (
      if not (is_free game [ d ]) then None
      else
        match d with
        | NW ->
            (* Either the other player is at direction N or W of the active player *)
            if
              not (is_free game [ N ])
              (* Inactive player is at the N of active player*)
            then
              if
                can_pass game [ N; W ]
                (* Active player can go at N then W *)
                && not (can_pass game [ N; N ])
                (* There is a wall preventing the active player to jump directly over the inactive player *)
              then Some (target_pos game [ N; W ])
              else None
            else if
              not (is_free game [ W ])
              (* Inactive player is at the W of active player *)
            then
              if can_pass game [ W; N ] && not (can_pass game [ W; W ]) then
                Some (target_pos game [ N; W ])
              else None
            else None
        | SW ->
            if not (is_free game [ S ]) then
              if can_pass game [ S; W ] && not (can_pass game [ S; S ]) then
                Some (target_pos game [ S; W ])
              else None
            else if not (is_free game [ W ]) then
              if can_pass game [ W; S ] && not (can_pass game [ W; W ]) then
                Some (target_pos game [ S; W ])
              else None
            else None
        | SE ->
            if not (is_free game [ S ]) then
              if can_pass game [ S; E ] && not (can_pass game [ S; S ]) then
                Some (target_pos game [ S; E ])
              else None
            else if not (is_free game [ E ]) then
              if can_pass game [ E; S ] && not (can_pass game [ E; E ]) then
                Some (target_pos game [ S; E ])
              else None
            else None
        | NE ->
            if not (is_free game [ N ]) then
              if can_pass game [ N; E ] && not (can_pass game [ N; N ]) then
                Some (target_pos game [ N; E ])
              else None
            else if not (is_free game [ E ]) then
              if can_pass game [ E; N ] && not (can_pass game [ E; E ]) then
                Some (target_pos game [ N; E ])
              else None
            else None
        | _ -> assert false)

(** Check if the active player can place this wall. *)
let place_wall_valid (game : t) (w : Board.wall) : bool =
  try
    (* TODO : check if there is a wall available *)
    let () = Board.add_wall game.board w in
    let game_is_blocked = is_blocked game in
    let () = Board.remove_wall game.board w in
    not game_is_blocked
  with _ -> false

(** Execute a given (valid) action. This modifies the state in place. *)
let execute_action (_game : t) (_act : action) : bool = failwith "todo"

(** Generate the list of all VALID actions. *)
let generate_actions (_game : t) : action list = failwith "todo"
