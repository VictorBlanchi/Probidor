open Quoridor
open Sockets
open Lwt.Syntax

(** This is the game engine/server. Its functions are to :
    - Handle the connections with the players.
    - Keep track of the game state.
    - Make sure the players don't make an illegal move. 
  Each player communicates only with the server (players don't directly communicate with each other). *)

(******************************************************************************************************)
(** Command line interface. *)

(** Message printed in case of a malformed command line, or when help is requested. *)
let usage_msg = "engine [--verbose] [--port 1234]"

(** Flag to control the amount of information printed by the engine. *)
let verbose = ref false

(** The port we use to listen for websocket connections. *)
let port = ref 8000

(** All command-line options. *)
let cli_options =
  [ ("--verbose", Arg.Set verbose, "Output debug information")
  ; ("--port", Arg.Set_int port, "Set the port used to listen for incoming websocket connections")
  ]

(*****************************************************************************************************)
(** Engine parameters. *)

(* The engine runs in a reader monad that has access to the player socket connections.
   It also runs in Lwt. *)
type 'a m = Server.connection * Server.connection -> 'a Lwt.t

module M : Engine.Monad with type 'a t = 'a m = struct
  type 'a t = 'a m

  let return x _ = Lwt.return x

  let bind m f conns =
    let* x = m conns in
    f x conns
end

(** Lift a computation from Lwt to our custom monad. *)
let lift (m : 'a Lwt.t) : 'a M.t = fun _ -> m

(** Run a computation in our custom monad. *)
let run connA connB (m : 'a M.t) : 'a = Lwt_main.run @@ m (connA, connB)

module L : Engine.Logger with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t
  type level = Info | Debug | Error [@@deriving show]

  (** Depending on the [verbose] flag, print a formatted string or do nothing. *)
  let log (level : level) fmt =
    Format.ksprintf
      begin
        fun str ->
          if !verbose then lift @@ Lwt_io.printf "[%s] %s\n" (show_level level) str else M.return ()
      end
      fmt
end

module C : Engine.Connection with type 'a t = 'a M.t = struct
  type 'a t = 'a M.t

  let receive_request player (connA, connB) =
    match player with
    | State.PlayerA -> Server.receive_request connA
    | State.PlayerB -> Server.receive_request connB

  let send_response player resp (connA, connB) =
    match player with
    | State.PlayerA -> Server.send_response connA resp
    | State.PlayerB -> Server.send_response connB resp
end

(* Instantiate the engine. *)
module E = Engine.Make (M) (L) (C)

let () =
  (* Parse command line arguments. *)
  Arg.parse cli_options (fun _ -> Arg.usage cli_options usage_msg) usage_msg;
  let uri = Uri.make ~scheme:"http" ~host:"localhost" ~port:!port () in
  (* Connect to the players *)
  let connA, connB =
    (* This part runs in Lwt.t instead of M.t because we don't have connections to the players yet. *)
    Lwt_main.run
      begin
        let* () = Lwt_io.printf "Listening for players on %s.\n" (Uri.to_string uri) in
        let* stream = Server.listen uri in
        let* connA = Lwt_stream.get stream in
        let* connB = Lwt_stream.get stream in
        let* () = Lwt_io.printf "Connected to players.\n" in
        Lwt.return (Option.get connA, Option.get connB)
      end
  in
  run connA connB @@ E.play ()
