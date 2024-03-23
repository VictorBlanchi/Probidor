(* This file defines the representation of the game's state used in the engine. *)

(* *)
(*
type player =
  { pawn_pos: position 
  ; remaining_walls: int }

(** Player 1 starts in the middle of the top row (0), and player 2 starts in the middle of the bottom row.
    The board always has an odd number of columns.  *)
type t = 
  { rows: int
  ; columns: int
  ; player_1 : player
  ; player_2 : player
  ; board: G.t }


(* Create a new game. The number of columns has to be odd. *)
let make rows columns wall_count = 
  if rows < 0 then
    raise (Invalid_argument "rows")
  else if columns < 0 || (columns mod 2 = 0) then
    raise (Invalid_argument "columns")
  else if wall_count < 0 then
    raise (Invalid_argument "wall_count")
  else 
    let player_1 = {} in 
    let player_2 = {} in
    let board = () in
    { rows; columns ; player_1 ; player_2 ; board }
    *)