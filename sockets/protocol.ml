(* This file defines the protocol that the engine (server) and
     players (clients) use to communicate (via sockets). *)
open Quoridor

type move = MovePawn of State.direction | PlaceWall of Board.wall

(** The type of requests players send to the engine. *)
type request =
  | NewPlayer
      (** This is the first message a player sends to the engine.
          As for now this is not strictly necessary, but in the future the 
          player might ask stuff to the engine here (e.g. ask to start first/second).
          The engine will answer with the game info and pawn position for the player. *)
  | DoMove of move
      (** Ask to do a given move. The engine will answer with an error if the move is invalid.
          Otherwise the engine will answer with the opponent's move, or with Win. *)
  | ValidMoves
      (** Ask for the list of all valid moves the player can make. 
          The player can expect an immediate response to this request. *)

(** The type of responses the engine sends to players. *)
type response =
  | Welcome of
      { rows : int
      ; cols : int
      ; wall_count : int
      ; wall_length : int
      ; pawn_pos : Board.pos
      }
      (** Give a player the information about the current game, 
          and tell him whether he is first or second to play. *)
  | OpponentMove of move  (** Tell what move the opponent made. *)
  | YouWon  (** You won. *)
  | MoveList of move list  (** Answer with a list of moves. *)
  | OpponentWon of move  (** The opponent made the given move and won. *)
  | Error of string  (** The request gave an error. *)

let direction_to_string (dir : State.direction) : string =
  match dir with
  | N -> "n"
  | NW -> "nw"
  | W -> "w"
  | SW -> "sw"
  | S -> "s"
  | SE -> "se"
  | E -> "e"
  | NE -> "ne"

let direction_from_string (dir : string) : State.direction =
  match dir with
  | "n" -> N
  | "nw" -> NW
  | "w" -> W
  | "sw" -> SW
  | "s" -> S
  | "se" -> SE
  | "e" -> E
  | "ne" -> NE
  | _ -> raise (Invalid_argument "direction_from_string")

(** Parse a move from Json.
     Raises [Yojson.Basic.Util.Type_error] on invalid input. *)
let decode_move (_json : Yojson.Basic.t) : (move, exn) Result.t =
  failwith "hello"

(** Parse a request from Json.
      Raises [Yojson.Basic.Util.Type_error] on invalid input. *)
(*let decode_request (json : Yojson.Basic.t) : request =
    let open Yojson.Basic.Util in
    let action = json |> member "action" |> to_string in
    match action with
    | "new-player" -> NewPlayer
    | "move-pawn" ->
        let player = json |> member "player" |> to_int in
        let dir = json |> member "dir" |> to_string |> direction_from_string in
        MovePawn { player; dir }
    | "place-wall" ->
        let player = json |> member "player" |> to_int in
        let orient =
          json |> member "orient" |> to_string |> orientation_from_string
        in
        let row = json |> member "row" |> to_int in
        let col = json |> member "col" |> to_int in
        PlaceWall { player; orient; row; col }
    | _ -> raise (Type_error ("Invalid request format", json))

  (** Encode a request as a Json object.
      This should be the exact inverse of decode_request. *)
  let encode_request (req : request) : Yojson.Basic.t =
    match req with
    | NewPlayer -> `Assoc [ ("action", `String "new-player") ]
    | MovePawn i ->
        `Assoc
          [ ("action", `String "move-pawn")
          ; ("player", `Int i.player)
          ; ("dir", `String (direction_to_string i.dir))
          ]
    | PlaceWall i ->
        `Assoc
          [ ("action", `String "place-wall")
          ; ("player", `Int i.player)
          ; ("orient", `String (orientation_to_string i.orient))
          ; ("row", `Int i.row)
          ; ("col", `Int i.col)
          ]

  (** Encode a response as a status code and a json object. *)
  let encode_response (resp : response) : Cohttp.Code.status_code * Yojson.Basic.t
      =
    match resp with
    | Ok -> (`OK, `Null)
    | Error err -> (`Bad_request, `String err)
    | Welcome i ->
        ( `OK
        , `Assoc
            [ ("player", `Int i.player)
            ; ("rows", `Int i.rows)
            ; ("cols", `Int i.cols)
            ; ("wall-count", `Int i.wall_count)
            ] )

  (** Raises [Yojson.Basic.Util.Type_error] on invalid input. *)
  let decode_response (status_code : Cohttp.Code.status_code)
      (json : Yojson.Basic.t) : response =
    let open Yojson.Basic.Util in
    match (status_code, json) with
    | `Bad_request, _ ->
        let err = json |> to_string in
        Error err
    | `OK, `Null -> Ok
    | `OK, json ->
        let player = json |> member "player" |> to_int in
        let rows = json |> member "rows" |> to_int in
        let cols = json |> member "cols" |> to_int in
        let wall_count = json |> member "wall-count" |> to_int in
        Welcome { player; rows; cols; wall_count }
    | _, json -> raise (Type_error ("Invalid response format", json))
*)
