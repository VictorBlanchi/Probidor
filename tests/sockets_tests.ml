(*open Http
  open Protocol
  open QCheck2

  module Gen_syntax = struct
    let ( let* ) = Gen.( let* )
    let return = Gen.return
  end

  (*************************************************************************************)
  (* Generators *)

  let direction_gen : direction Gen.t = Gen.oneofl [ N; S; W; E; NE; NW; SW; SE ]
  let orientation_gen : orientation Gen.t = Gen.oneofl [ Vertical; Horizontal ]

  let request_gen : request Gen.t =
    let open Gen_syntax in
    Gen.oneof
      [ Gen.return NewPlayer
      ; (let* player = Gen.int in
         let* dir = direction_gen in
         Gen.return (MovePawn {player; dir}))
      ; (let* player = Gen.int in
         let* orient = orientation_gen in
         let* row = Gen.int in
         let* col = Gen.int in
         Gen.return (PlaceWall {player; orient; row; col}))
      ]

  let response_gen : response Gen.t =
    let open Gen_syntax in
    Gen.oneof
      [ Gen.return Ok
      ; (let* player = Gen.int in
         let* rows = Gen.int in
         let* cols = Gen.int in
         let* wall_count = Gen.int in
         Gen.return (Welcome {player; rows; cols; wall_count}))
      ; (let* err = Gen.string_small_of Gen.printable in
         Gen.return (Error err))
      ]

  (*************************************************************************************)
  (* Tests *)

  let uncurry f (a, b) = f a b

  let tests =
    [ Test.make ~name:"direction_to_from_string" direction_gen (fun dir ->
          direction_from_string (direction_to_string dir) = dir)
    ; Test.make ~name:"orientation_to_from_string" orientation_gen (fun orient ->
          orientation_from_string (orientation_to_string orient) = orient)
    ; Test.make ~name:"request_encode_decode" request_gen (fun req ->
          decode_request (encode_request req) = req)
    ; Test.make ~name:"response_encode_decode" response_gen (fun resp ->
          uncurry decode_response (encode_response resp) = resp)
    ]

  let () = QCheck_base_runner.run_tests_main tests*)
