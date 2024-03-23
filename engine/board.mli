(* This module defines the central data structure of the engine.
   The board tracks the walls placed so far, but does not deal with pawns. 
   This is an imperative data structure : most operations modify it in place. *)

(** The abstract type of boards. *)
type t

(** The type of a position on the board, represented as a pair of coordinates (row, col). 
    Coordinates start at 0 (inclusive). 
    The rows start at the top and go down, and the columns start at the left and go right. *)
type pos = int * int

exception Bad_wall_placement

val rows : t -> int
val columns : t -> int

(** Create a fresh board. A board always has an odd number of columns. *)
val make : rows:int -> columns:int -> t

(** Add a vertical or horizontal wall to the board. 
    Raises [Bad_wall_placement] if the wall placement is invalid 
    (i.e. overlaps with another wall or goes out of the board). *)
val add_wall : t -> horizontal:bool -> int -> pos

(** Is there a path between two positions ? *)
val reachable : t -> pos -> pos -> bool
(** Same as [reachable], but the destination is specified as a set of positions. *)
val reachable_pred : t -> pos -> (pos -> bool) -> bool