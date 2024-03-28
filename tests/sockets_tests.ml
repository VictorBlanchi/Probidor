open Quoridor
open Sockets
open QCheck2

module Gen_syntax = struct
  let ( let* ) = Gen.( let* )
  let return = Gen.return
end

(*************************************************************************************)
(* Generators *)

let pos_gen : Board.pos Gen.t = Gen.pair Gen.int Gen.int

let wall_gen : Board.wall Gen.t =
  let open Gen_syntax in
  let* horizontal = Gen.bool in
  let* length = Gen.int in
  let* pos = pos_gen in
  return Board.{ horizontal; length; pos }

let dir_gen : State.direction Gen.t =
  Gen.oneofl State.[ N; NW; W; SW; S; SE; E; NE ]

let action_gen : State.action Gen.t =
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

let request_gen : Protocol.request Gen.t =
  let open Gen_syntax in
  Gen.oneof
    [ return Protocol.NewPlayer
    ; begin
        let* act = action_gen in
        return @@ Protocol.DoAction act
      end
    ; return Protocol.ValidActions
    ]

let response_gen : Protocol.response Gen.t =
  let open Gen_syntax in
  Gen.oneof
    [ begin
        let* rows = Gen.int in
        let* cols = Gen.int in
        let* wall_count = Gen.int in
        let* wall_length = Gen.int in
        let* your_pawn = pos_gen in
        let* opp_pawn = pos_gen in
        let* you_start = Gen.bool in
        return
          (Protocol.Welcome
             { rows
             ; cols
             ; wall_count
             ; wall_length
             ; your_pawn
             ; opp_pawn
             ; you_start
             })
      end
    ; begin
        let* action = action_gen in
        let* win = Gen.bool in
        return @@ Protocol.OppAction { action; win }
      end
    ; return Protocol.YouWin
    ; begin
        let* actions = Gen.small_list action_gen in
        return @@ Protocol.ActionList actions
      end
    ; begin
        let* msg = Gen.string_small_of Gen.printable in
        return @@ Protocol.Error msg
      end
    ]

(*************************************************************************************)
(* Tests *)

let tests =
  [ Test.make ~name:"pos_encode_decode" pos_gen
      begin
        fun pos -> Protocol.decode_pos @@ Protocol.encode_pos pos = pos
      end
  ; Test.make ~name:"dir_encode_decode" dir_gen
      begin
        fun dir ->
          Protocol.decode_direction (Protocol.encode_direction dir) = dir
      end
  ; Test.make ~name:"wall_encode_decode" wall_gen
      begin
        fun wall -> Protocol.decode_wall (Protocol.encode_wall wall) = wall
      end
  ; Test.make ~name:"action_encode_decode" action_gen
      begin
        fun act -> Protocol.decode_action (Protocol.encode_action act) = act
      end
  ; Test.make ~name:"request_encode_decode" request_gen
      begin
        fun req -> Protocol.decode_request (Protocol.encode_request req) = req
      end
  ; Test.make ~name:"response_encode_decode" response_gen
      begin
        fun resp ->
          Protocol.decode_response (Protocol.encode_response resp) = resp
      end
  ]

let () = QCheck_base_runner.run_tests_main tests
