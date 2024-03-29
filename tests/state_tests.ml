open Quoridor
open State
open QCheck2

module Gen_syntax = struct
  let ( let* ) = Gen.( let* )
  let return = Gen.return
end

(*************************************************************************************)
(* Generators *)

let pos_gen : Board.pos Gen.t =
  Gen.pair Gen.small_signed_int Gen.small_signed_int

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

let valid_action_gen (game : t) : action Gen.t =
  Gen.oneofl @@ valid_actions game

let new_game_gen : t Gen.t =
  let open Gen_syntax in
  let* r = Gen.int_range 5 20 in
  let* c = Gen.int_range 2 10 in
  let* w_count = Gen.small_nat in
  let* w_len = Gen.small_nat in
  let* to_play = player_gen in
  return @@ make r ((2 * c) + 1) w_count w_len to_play

(*************************************************************************************)
(* Printers *)

let pos_print : Board.pos Print.t = Print.pair Print.int Print.int

let wall_print : Board.wall Print.t =
  let open Board in
  fun wall ->
    Format.sprintf "{ horizontal:%b; length:%d; pos:%s }" wall.horizontal
      wall.length
    @@ pos_print wall.pos

let board_print : Board.t Print.t =
  let open Board in
  fun board ->
    Format.sprintf "{ rows:%d; cols:%d }" (rows board) (columns board)

let direction_print : direction Print.t = function
  | N -> "N"
  | NW -> "NW"
  | W -> "W"
  | SW -> "SW"
  | S -> "S"
  | SE -> "SE"
  | E -> "E"
  | NE -> "NE"

let action_print : action Print.t = function
  | MovePawn d -> Format.sprintf " MovePawn %s" (direction_print d)
  | PlaceWall w -> Format.sprintf "PlaceWall %s" (wall_print w)

let player_data_print : player_data Print.t =
 fun player ->
  Format.sprintf "{ pawn_pos:%s; remaining_walls:%d}"
    (pos_print player.pawn_pos)
    player.remaining_walls

let player_print : player Print.t = function
  | PlayerA -> Format.sprintf "PlayerA"
  | PlayerB -> Format.sprintf "PlayerB"

let game_print : t Print.t =
 fun game ->
  Format.sprintf
    "{ board:%s; wall_length:%d; player_A:%s; player_B:%s; to_play:%s}"
    (board_print game.board) game.wall_length
    (player_data_print game.player_A)
    (player_data_print game.player_B)
    (player_print game.to_play)

(***************************************************************************************)
(* Tests *)

let tests =
  [ Test.make ~name:"execute_valid_action" ~count:100
      ~print:(Print.pair game_print action_print)
      (let open Gen_syntax in
       let* game = new_game_gen in
       Gen.pair (return game) (valid_action_gen game))
      begin
        fun (game, action) ->
          try
            execute_action game action;
            true
          with _ -> false
      end
  ]

let () = QCheck_base_runner.run_tests_main tests
