(* This file defines the protocol that the engine (server) and
     players (clients) use to communicate via sockets. *)
open Quoridor

(** The type of requests players send to the engine. *)
type request =
  | NewPlayer
      (** This is the first message a player sends to the engine.
          As for now this is not strictly necessary, but in the future the 
          player might ask stuff to the engine here (e.g. ask to start first/second).
          The engine will answer with the game info and pawn position for the player. *)
  | DoAction of State.action
      (** Ask to do a given action. The engine will answer with an error if the action is invalid.
          Otherwise the engine will answer with the opponent's action, or with Win. *)
  | ValidActions
      (** Ask for the list of all valid actions the player can make. 
          The player can expect an immediate response to this request. *)

(** The type of responses the engine sends to players. *)
type response =
  | Welcome of
      { rows : int
      ; cols : int
      ; wall_count : int
      ; wall_length : int
      ; your_pawn : Board.pos
      ; opp_pawn : Board.pos
      ; you_start : bool
      }
      (** Give a player the information about the current game, 
          and whether he is first or second to play. *)
  | OppAction of { action : State.action; win : bool }
      (** Tell what action the opponent made, and whether this action made him win. *)
  | YouWin  (** You won. *)
  | ActionList of State.action list  (** Answer with a list of actions. *)
  | Error of string  (** Invalid request : explain why. *)

(********************************************************************)
(** Encoding to JSON. *)

let encode_direction (dir : State.direction) : Yojson.Basic.t =
  let str =
    match dir with
    | N -> "n"
    | NW -> "nw"
    | W -> "w"
    | SW -> "sw"
    | S -> "s"
    | SE -> "se"
    | E -> "e"
    | NE -> "ne"
  in
  `String str

let encode_pos ((i, j) : Board.pos) : Yojson.Basic.t = `Assoc [ ("row", `Int i); ("col", `Int j) ]

let encode_wall (wall : Board.wall) : Yojson.Basic.t =
  `Assoc
    [ ("horizontal", `Bool wall.horizontal)
    ; ("length", `Int wall.length)
    ; ("pos", encode_pos wall.pos)
    ]

let encode_action (act : State.action) : Yojson.Basic.t =
  match act with
  | MovePawn dir -> `Assoc [ ("action", `String "move-pawn"); ("dir", encode_direction dir) ]
  | PlaceWall wall -> `Assoc [ ("action", `String "place-wall"); ("wall", encode_wall wall) ]

let encode_request (req : request) : Yojson.Basic.t =
  match req with
  | NewPlayer -> `Assoc [ ("request", `String "new-player") ]
  | DoAction act -> `Assoc [ ("request", `String "do-action"); ("action", encode_action act) ]
  | ValidActions -> `Assoc [ ("request", `String "valid-actions") ]

let encode_response (resp : response) : Yojson.Basic.t =
  match resp with
  | Welcome info ->
      `Assoc
        [ ("response", `String "welcome")
        ; ("rows", `Int info.rows)
        ; ("cols", `Int info.cols)
        ; ("wall-count", `Int info.wall_count)
        ; ("wall-length", `Int info.wall_length)
        ; ("your-pawn", encode_pos info.your_pawn)
        ; ("opp-pawn", encode_pos info.opp_pawn)
        ; ("you-start", `Bool info.you_start)
        ]
  | OppAction { action; win } ->
      `Assoc
        [ ("response", `String "opp-action"); ("action", encode_action action); ("win", `Bool win) ]
  | YouWin -> `Assoc [ ("response", `String "you-win") ]
  | ActionList acts ->
      `Assoc
        [ ("response", `String "action-list"); ("actions", `List (List.map encode_action acts)) ]
  | Error err -> `Assoc [ ("response", `String "error"); ("msg", `String err) ]

(********************************************************************)
(** Parsing from JSON.
    These functions raise [Yojson.Basic.Util.Type_error] on invalid input. *)

let decode_direction (json : Yojson.Basic.t) : State.direction =
  let open Yojson.Basic.Util in
  match json |> to_string with
  | "n" -> N
  | "nw" -> NW
  | "w" -> W
  | "sw" -> SW
  | "s" -> S
  | "se" -> SE
  | "e" -> E
  | "ne" -> NE
  | _ -> raise @@ Yojson.Basic.Util.Type_error ("Invalid direction", json)

let decode_pos (json : Yojson.Basic.t) : Board.pos =
  let open Yojson.Basic.Util in
  let i = json |> member "row" |> to_int in
  let j = json |> member "col" |> to_int in
  (i, j)

let decode_wall (json : Yojson.Basic.t) : Board.wall =
  let open Yojson.Basic.Util in
  let horizontal = json |> member "horizontal" |> to_bool in
  let length = json |> member "length" |> to_int in
  let pos = json |> member "pos" |> decode_pos in
  { horizontal; length; pos }

let decode_action (json : Yojson.Basic.t) : State.action =
  let open Yojson.Basic.Util in
  match json |> member "action" |> to_string with
  | "move-pawn" ->
      let dir = json |> member "dir" |> decode_direction in
      MovePawn dir
  | "place-wall" ->
      let wall = json |> member "wall" |> decode_wall in
      PlaceWall wall
  | _ -> raise @@ Yojson.Basic.Util.Type_error ("Invalid action type.", json)

let decode_request (json : Yojson.Basic.t) : request =
  let open Yojson.Basic.Util in
  match json |> member "request" |> to_string with
  | "new-player" -> NewPlayer
  | "do-action" ->
      let act = json |> member "action" |> decode_action in
      DoAction act
  | "valid-actions" -> ValidActions
  | _ -> raise @@ Yojson.Basic.Util.Type_error ("Invalid request type.", json)

let decode_response (json : Yojson.Basic.t) : response =
  let open Yojson.Basic.Util in
  match json |> member "response" |> to_string with
  | "welcome" ->
      let rows = json |> member "rows" |> to_int in
      let cols = json |> member "cols" |> to_int in
      let wall_count = json |> member "wall-count" |> to_int in
      let wall_length = json |> member "wall-length" |> to_int in
      let your_pawn = json |> member "your-pawn" |> decode_pos in
      let opp_pawn = json |> member "opp-pawn" |> decode_pos in
      let you_start = json |> member "you-start" |> to_bool in
      Welcome { rows; cols; wall_count; wall_length; your_pawn; opp_pawn; you_start }
  | "opp-action" ->
      let action = json |> member "action" |> decode_action in
      let win = json |> member "win" |> to_bool in
      OppAction { action; win }
  | "you-win" -> YouWin
  | "action-list" ->
      let actions = json |> member "actions" |> to_list |> List.map decode_action in
      ActionList actions
  | "error" ->
      let msg = json |> member "msg" |> to_string in
      Error msg
  | _ -> raise @@ Yojson.Basic.Util.Type_error ("Invalid response type.", json)
