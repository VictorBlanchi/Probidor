(* This file defines the HTTP protocol that the engine (server) and
   players (clients) use to communicate. *)

(** The directions a pawn can move in. *)
type direction = N | NW | W | SW | S | SE | E | NE

(** The orientation of a wall. *)
type orientation = Vertical | Horizontal

(** The type of requests players send to the engine. *)
type request =
  | NewPlayer
      (** This is the first message a player sends to the engine.
      The engine will answer with the game info (board size & wall count), and player identifier. *)
  | MovePawn of {player: int; dir: direction}  (** Move the pawn. *)
  | PlaceWall of {player: int; orient: orientation; row: int; col: int}
      (** Place a wall. *)

(** The type of responses the engine sends to players. *)
type response =
  | Welcome of {player: int; rows: int; cols: int; wall_count: int}
      (** Give a player it's identifier and the information about the current game. *)
  | Ok  (** Just acknowledge the request. *)
  | Error of string  (** The request gave an error. *)

let direction_to_string (dir : direction) : string =
  match dir with
  | N ->
      "n"
  | NW ->
      "nw"
  | W ->
      "w"
  | SW ->
      "sw"
  | S ->
      "s"
  | SE ->
      "se"
  | E ->
      "e"
  | NE ->
      "ne"

let direction_from_string (dir : string) : direction =
  match dir with
  | "n" ->
      N
  | "nw" ->
      NW
  | "w" ->
      W
  | "sw" ->
      SW
  | "s" ->
      S
  | "se" ->
      SE
  | "e" ->
      E
  | "ne" ->
      NE
  | _ ->
      raise (Invalid_argument "direction_from_string")

let orientation_to_string (orient : orientation) : string =
  match orient with Vertical -> "vertical" | Horizontal -> "horizontal"

let orientation_from_string (orient : string) : orientation =
  match orient with
  | "vertical" ->
      Vertical
  | "horizontal" ->
      Horizontal
  | _ ->
      raise (Invalid_argument "orientation_from_string")

(** Parse a request from Json.
    Raises [Yojson.Basic.Util.Type_error] on invalid input. *)
let decode_request (json : Yojson.Basic.t) : request =
  let open Yojson.Basic.Util in
  let action = json |> member "action" |> to_string in
  match action with
  | "new-player" ->
      NewPlayer
  | "move-pawn" ->
      let player = json |> member "player" |> to_int in
      let dir = json |> member "dir" |> to_string |> direction_from_string in
      MovePawn {player; dir}
  | "place-wall" ->
      let player = json |> member "player" |> to_int in
      let orient =
        json |> member "orient" |> to_string |> orientation_from_string
      in
      let row = json |> member "row" |> to_int in
      let col = json |> member "col" |> to_int in
      PlaceWall {player; orient; row; col}
  | _ ->
      raise (Type_error ("Invalid request format", json))

(** Encode a request as a Json object.
    This should be the exact inverse of decode_request. *)
let encode_request (req : request) : Yojson.Basic.t =
  match req with
  | NewPlayer ->
      `Assoc [ ("action", `String "new-player") ]
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
  | Ok ->
      (`OK, `Null)
  | Error err ->
      (`Bad_request, `String err)
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
  | `OK, `Null ->
      Ok
  | `OK, json ->
      let player = json |> member "player" |> to_int in
      let rows = json |> member "rows" |> to_int in
      let cols = json |> member "cols" |> to_int in
      let wall_count = json |> member "wall-count" |> to_int in
      Welcome {player; rows; cols; wall_count}
  | _, json ->
      raise (Type_error ("Invalid response format", json))
