(* This file defines the representation of the game's state used in the engine. *)

(** The directions a pawn can move in. *)
type direction = 
  | N
  | NW
  | W
  | SW
  | S 
  | SE
  | E 
  | NE

(** An action a player can take during their turn. *)
type action = 
  | MovePawn of direction
  | PlaceWall of Board.wall

(** The game state specific to a player. *)
type player_data =
  { pawn_pos: Board.pos 
  ; remaining_walls: int }

(** The state of the game.
    The board always has an odd number of columns.  *)
type t = 
  { board: Board.t
  ; player_A: player_data
  ; player_B: player_data
  ; to_play: bool (** [false] means player A is to play, and [true] means player B is to play*) }


(** Create a new game. The number of columns has to be odd. 
    Player A starts in the middle of the top row (0), and player B starts in the middle of the bottom row. *)
let make rows columns wall_count to_play = 
  if rows < 0 then
    raise (Invalid_argument "rows")
  else if columns < 0 || (columns mod 2 = 0) then
    raise (Invalid_argument "columns")
  else if wall_count < 0 then
    raise (Invalid_argument "wall_count")
  else 
    let player_A = { pawn_pos = (0, columns / 2); remaining_walls = wall_count } in 
    let player_B = { pawn_pos = (rows - 1, columns / 2); remaining_walls = wall_count } in
    let board = Board.make ~rows ~columns in
    { player_A ; player_B ; board ; to_play }
    
(** Check if a given action is valid. *)
let action_valid (game : t) (act : action) : bool = 
  failwith "todo"

(** Execute a given (valid) action. This modifies the state in place. *)
let execute_action (game : t) (act : action) : bool = 
  failwith "todo"

(** Generate the list of all VALID actions. *)
let generate_actions (game : t) : action list = 
  failwith "todo"