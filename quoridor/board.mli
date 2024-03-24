(* This module defines the central data structure of the engine.
   The board tracks the walls placed so far, but does not deal with pawns. 
   This is an imperative data structure : most operations modify it in place. *)


(** The type of a position on the board, represented as a pair of coordinates [(row, col)]. 
    Coordinates are in the range [0..rows-1] and [0..columns-1]. 
    The rows start at the top and go down, and the columns start at the left and go right. *)
type pos = int * int

(** The type of a wall on the board.
    The length of a wall should always be positive.
    The position [pos] is the upper-left cell among those directly adjacent to the wall. *)
type wall =
  { horizontal: bool
  ; length: int 
  ; pos: pos }

(** The abstract type of boards. *)
type t

exception Wall_out_of_bounds
exception Wall_overlap
exception Wall_missing

(* Get the size of a board. *)
val rows : t -> int
val columns : t -> int

(** Create a fresh board. A board can have any number of rows and columns > 0. *)
val make : rows:int -> columns:int -> t

(** Get the edges corresponding to a wall, even if it is out of bounds. *)
val wall_edges : wall -> (pos * pos) list

(** Is a position in the bounds of the board ? *)
val pos_in_board : t -> pos -> bool

(** Is a wall in the bounds of the board ? *)
val wall_in_board : t -> wall -> bool

(** Add a vertical or horizontal wall to the board. 
    Raises [Wall_out_of_bounds] if the wall placement is invalid.
    Raises [Wall_overlap] if the wall would overlap with another. *)
val add_wall : t -> wall -> unit

(** Add a vertical or horizontal wall to the board. 
    Raises [Wall_out_of_bounds] if the wall placement is invalid.
    Raises [Wall_missing] if there is no wall here. *)
val remove_wall : t -> wall -> unit

(** Test if there a path between two positions, 
    where the destination is specified as a predicate on positions. *)
val reachable : t -> pos -> (pos -> bool) -> bool