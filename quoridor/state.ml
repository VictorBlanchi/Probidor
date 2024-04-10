(* This file defines the representation of the game's state used in the engine. *)

(** Type representing different illegal move scenarios in a game. *)
type illegal_move =
  | OutOfBounds  (** The move is outside the bounds of the board. *)
  | PlayerCollision  (** Another player occupies the destination. *)
  | WallCollision
  | NoPlayerToJumpOver
  | NoWallForDiagonalMove  (** No wall behind the opponent for a diagonal move.*)
[@@deriving show]

exception IllegalMove of illegal_move

(** Type representing different scenarios of illegal wall placement in a game.*)
type illegal_wall =
  | OutOfWalls  (** The player has no more walls available to place.*)
  | BlocksGame  (** Placing the wall would block the game, rendering it unplayable.*)
  | OutOfBounds  (** Part of the wall is out of bounds. *)
  | Overlap  (** The wall overlap with an existing wall. *)
[@@deriving show]

exception IllegalWall of illegal_wall

(** A tag identifying the two players.
    Player A starts on top of the board, and player B start at the bottom of the board. *)
type player = PlayerA | PlayerB [@@deriving show]

(** The directions a pawn can move in. *)
type direction = N | NW | W | SW | S | SE | E | NE [@@deriving show]

(** An action a player can take during their turn. *)
type action =
  | MovePawn of direction  (** Move the pawn of the current player. *)
  | PlaceWall of Board.wall  (** Place a wall for the current player. *)
[@@deriving show]

(** The game state specific to a player. *)
type player_data = { mutable pawn_pos : Board.pos; mutable remaining_walls : int } [@@deriving show]

(** The state of the game.
    The board always has an odd number of columns. *)
type t =
  { board : Board.t [@opaque]
  ; wall_length : int
  ; player_A : player_data
  ; player_B : player_data
  ; mutable to_play : player
  }
[@@deriving show]

(** Turn PlayerA to PlayerB and vice-versa. *)
let swap_player (p : player) : player = match p with PlayerA -> PlayerB | PlayerB -> PlayerA

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
    let player_A = { pawn_pos = (0, columns / 2); remaining_walls = wall_count } in
    let player_B = { pawn_pos = (rows - 1, columns / 2); remaining_walls = wall_count } in
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
    - [split_diag_dir 'N'] raises [Invalid_argument]. *)
let split_diag_dir (d : direction) : direction * direction =
  match d with
  | N | W | S | E -> raise @@ Invalid_argument "split_diag_dir"
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

(** Check that the position the active player wants to reach is not occupied. 
    Raise an error if the position does not exist in the board *)
let is_free (game : t) (d : direction list) : bool =
  let p' = target_pos game d in
  if Board.pos_in_board game.board p'
  then p' <> pos_active game && p' <> pos_inactive game
  else raise @@ IllegalMove OutOfBounds

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
let win_pos_B (_game : t) (p : Board.pos) : bool = match p with 0, _ -> true | _ -> false

(** Winning positions of active player. *)
let win_pos (game : t) : Board.pos -> bool =
  match game.to_play with PlayerA -> win_pos_A game | PlayerB -> win_pos_B game

(** Check whether the active player wins. *)
let win (game : t) : bool = win_pos game (pos_active game)

(** A game is blocked if player A or player B are not able to win. *)
let is_blocked (game : t) : bool =
  let able_to_win_A = Board.reachable game.board game.player_A.pawn_pos (win_pos_A game) in
  let able_to_win_B = Board.reachable game.board game.player_B.pawn_pos (win_pos_B game) in
  not (able_to_win_A && able_to_win_B)

(** Remaining walls of the active player. *)
let remaining_walls (game : t) : int =
  match game.to_play with
  | PlayerA -> game.player_A.remaining_walls
  | PlayerB -> game.player_B.remaining_walls

(** Decrement the number of remaining walls of the active player. Assert that there is at least a wall*)
let decrement_walls (game : t) : unit =
  let m = remaining_walls game in
  assert (m <= 0);
  match game.to_play with
  | PlayerA -> game.player_A.remaining_walls <- m - 1
  | PlayerB -> game.player_B.remaining_walls <- m - 1

(** Move the active player's pawn and return the new position of the pawn.
    Raises [IllegalMove] if the move is invalid. *)
let move_pawn (game : t) (d : direction) : Board.pos =
  match d with
  | (N | E | S | W) as d ->
      if is_free game [ d ]
      then begin
        if can_pass game [ d ] then target_pos game [ d ] else raise @@ IllegalMove WallCollision
      end
      else if (* The other player is next to us and maybe we can jump over him *)
              is_free game [ d; d ]
      then begin
        if can_pass game [ d; d ]
        then target_pos game [ d; d ]
        else raise @@ IllegalMove WallCollision
      end
      else raise @@ IllegalMove PlayerCollision
  | (NE | SE | SW | NW) as d -> begin
      if is_free game [ d ]
      then
        (* Perform d1 then d2. *)
        let diag_move d1 d2 =
          if is_free game [ d1 ]
          then raise @@ IllegalMove NoPlayerToJumpOver
          else if (* Inactive player at d1 from us *)
                  not @@ can_pass game [ d1; d2 ]
          then raise @@ IllegalMove WallCollision
          else if (* No wall preventing to perform d1 d2 *)
                  can_pass game [ d1; d1 ]
          then raise @@ IllegalMove NoWallForDiagonalMove
          else target_pos game [ d1; d2 ]
          (* A wall preventing us to jump directly over the inactive player *)
        in
        let d1, d2 = split_diag_dir d in
        if is_free game [ d1 ] then diag_move d2 d1 else diag_move d1 d2
      else raise @@ IllegalMove PlayerCollision
    end

(** Make the active player place a wall.
    Raises [IllegalWall] if the wall placement is invalid. 
    If [check_only] is set, only check that the placement is valid. *)
let place_wall ?(check_only = false) (game : t) (w : Board.wall) : unit =
  (* Helper function to add the wall. *)
  let add () =
    try Board.add_wall game.board w with
    | Board.WallOverlap -> raise @@ IllegalWall Overlap
    | Board.WallOutOfBounds -> raise @@ IllegalWall OutOfBounds
  in
  (* Helper function to remove the wall. *)
  let remove () =
    try Board.remove_wall game.board w
    with Board.WallMissing | Board.WallOutOfBounds -> failwith "place_wall"
  in
  if (* Check the active player has walls remaining. *)
     remaining_walls game <= 0
  then raise @@ IllegalWall OutOfWalls
  else add ();
  if (* Check the game is not blocked. *)
     is_blocked game
  then begin
    remove ();
    raise @@ IllegalWall BlocksGame
  end
  else if (* If we check only, remove the wall. *)
          check_only
  then remove ()

(** Execute an action. This modifies the state in place.
    If the action is invalid, raise [IllegalMove] or [IllegalWall], 
    and leave the game in a valid state (as if the action had not been executed). *)
let execute_action (game : t) (act : action) : unit =
  match act with
  | MovePawn dir -> (active_player game).pawn_pos <- move_pawn game dir
  | PlaceWall wall -> place_wall game wall

(** Generate the list of all VALID actions.
    This is not efficient (tries every action and keeps only valid ones). *)
let valid_actions (game : t) : action list =
  (* Generate valid moves. *)
  let directions = [ N; NW; W; SW; S; SE; E; NE ] in
  let valid_moves =
    directions
    |> List.filter
         begin
           fun dir ->
             try
               ignore (move_pawn game dir);
               true
             with IllegalMove _ -> false
         end
    |> List.map (fun dir -> MovePawn dir)
  in
  (* Generate valid wall placements. *)
  let valid_walls =
    Board.generate_walls game.board game.wall_length
    |> List.filter
         begin
           fun wall ->
             try
               place_wall ~check_only:true game wall;
               true
             with IllegalWall _ -> false
         end
    |> List.map (fun wall -> PlaceWall wall)
  in
  valid_moves @ valid_walls
