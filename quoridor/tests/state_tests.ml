open Quoridor
open State
open QCheck2

module Gen_syntax = struct
  let ( let* ) = Gen.( let* )
  let return = Gen.return
end

(*************************************************************************************)
(* Generators *)

let pos_gen : Board.pos Gen.t = Gen.pair Gen.small_signed_int Gen.small_signed_int

let wall_gen : Board.wall Gen.t =
  let open Gen_syntax in
  let* horizontal = Gen.bool in
  let* length = Gen.int_range 1 10 in
  let* pos = pos_gen in
  return { Board.horizontal; Board.length; Board.pos }

let dir_gen : direction Gen.t = Gen.oneofl [ N; NW; W; SW; S; SE; E; NE ]
let card_dir_gen : direction Gen.t = Gen.oneofl [ N; E; S; W ]
let diag_dir_gen : direction Gen.t = Gen.oneofl [ NW; SW; SE; NE ]
let player_gen : player Gen.t = Gen.oneofl [ PlayerA; PlayerB ]

let action_gen : action Gen.t =
  let open Gen_syntax in
  Gen.oneof
    [ begin
        let* dir = dir_gen in
        return @@ State.MovePawn dir
      end
    ; begin
        let* wall = wall_gen in
        return @@ State.PlaceWall wall
      end
    ]

(** Create a new game where no actions have been taken yet. *)
let new_game_gen : t Gen.t =
  let open Gen_syntax in
  let* rows = Gen.int_range 5 25 in
  let* columns = Gen.int_range 2 12 in
  let columns = (2 * columns) + 1 in
  let* wall_count = Gen.small_nat in
  let* wall_length = Gen.int_range 1 (min rows columns) in
  let* to_play = player_gen in
  return @@ make ~rows ~columns ~wall_count ~wall_length ~to_play

(*************************************************************************************)
(* Printers *)

let show_board (board : Board.t) : string =
  Format.sprintf "{ rows:%d; cols:%d }" (Board.rows board) (Board.columns board)

(***************************************************************************************)
(* Tests *)

(** Choose a random element in a list. *)
let choose (xs : 'a list) : 'a =
  let idx = Random.int (List.length xs) in
  List.nth xs idx

(** Play random valid moves up to [count] times. 
    Returns [false] if there was something unexpected. *)
let rec play_random (game : t) (count : int) : bool =
  if count <= 0
  then true
  else
    let acts = valid_actions game in
    if List.is_empty acts
    then true
    else begin
      (* Execute a random action and check that the active player changes accordingly. *)
      let to_play = game.to_play in
      execute_action game (choose acts);
      if game.to_play != State.swap_player to_play then false else play_random game (count - 1)
    end

let tests =
  [ (* Execute valid actions many times and check that no eror is thrown. *)
    Test.make ~name:"execute_valid_action" ~count:100 ~print:show new_game_gen
      begin
        fun game -> play_random game 10
      end
  ; (* Same as above, but test much more deeply. *)
    Test.make ~name:"execute_valid_action" ~count:10 ~print:show new_game_gen
      begin
        fun game -> play_random game 100
      end
  ]

let () = QCheck_base_runner.run_tests_main tests
