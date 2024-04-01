open Quoridor
open Board
open QCheck2

module Gen_syntax = struct
  let ( let* ) = Gen.( let* )
  let return = Gen.return
end

(*************************************************************************************)
(* Generators *)

let pos_gen : pos Gen.t = Gen.pair Gen.small_signed_int Gen.small_signed_int

let wall_gen : wall Gen.t =
  let open Gen_syntax in
  let* horizontal = Gen.bool in
  let* length = Gen.int_range 1 10 in
  let* pos = pos_gen in
  return { horizontal; length; pos }

let board_gen : t Gen.t =
  let open Gen_syntax in
  let* rows = Gen.int_range 1 50 in
  let* columns = Gen.int_range 1 50 in
  return (make ~rows ~columns)

(*************************************************************************************)
(* Printers *)

let pos_print : pos Print.t = Print.pair Print.int Print.int

let wall_print : wall Print.t =
 fun wall ->
  Format.sprintf "{ horizontal:%b; length:%d; pos:%s }" wall.horizontal
    wall.length (pos_print wall.pos)

let board_print : t Print.t =
 fun board -> Format.sprintf "{ rows:%d; cols:%d }" (rows board) (columns board)

(*************************************************************************************)
(* Tests *)

let tests =
  [ Test.make ~name:"wall_in_board_imp_pos_in_board"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (wall_in_board board wall);
          List.for_all
            (fun (p1, p2) -> pos_in_board board p1 && pos_in_board board p2)
            (wall_edges wall)
      end
  ; Test.make ~name:"pos_in_board_imp_wall_in_board"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume
            (List.for_all
               (fun (p1, p2) -> pos_in_board board p1 && pos_in_board board p2)
               (wall_edges wall));
          wall_in_board board wall
      end
  ; Test.make ~name:"add_wall_not_in_board"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (not (wall_in_board board wall));
          try
            add_wall board wall;
            false
          with WallOutOfBounds -> true
      end
  ; Test.make ~name:"remove_wall_not_in_board"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (not (wall_in_board board wall));
          try
            remove_wall board wall;
            false
          with WallOutOfBounds -> true
      end
  ; Test.make ~name:"add_wall"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (wall_in_board board wall);
          add_wall board wall;
          true
      end
  ; Test.make ~name:"remove_wall"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (wall_in_board board wall);
          try
            remove_wall board wall;
            false
          with WallMissing -> true
      end
  ; Test.make ~name:"add_remove_wall"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (wall_in_board board wall);
          add_wall board wall;
          remove_wall board wall;
          true
      end
  ; Test.make ~name:"add_add_wall"
      ~print:(Print.pair wall_print board_print)
      (Gen.pair wall_gen board_gen)
      begin
        fun (wall, board) ->
          assume (wall_in_board board wall);
          try
            add_wall board wall;
            add_wall board wall;
            false
          with WallOverlap -> true
      end
  ]

let () = QCheck_base_runner.run_tests_main tests
