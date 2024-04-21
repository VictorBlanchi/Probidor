(** This executable defines a small wrapper around Engine.ml(i). *)

open Sockets
open Lwt.Syntax

(** Message printed in case of a malformed command line, or when help is requested. *)
let usage_msg = "engine [--verbose]"

(** Flag to control the amount of information printed by the engine. *)
let verbose = ref false

(** All command-line options. *)
let cli_options = [ ("--verbose", Arg.Set verbose, "Output debug information") ]

(** The game engine communicates to players using sockets. *)
module Conn : Engine.Connection = struct
  type t = Server.connection

  let send_response = Server.send_response
  let receive_request = Server.receive_request

  let connect () =
    let* conns = Server.connect_to_clients 2 in
    match conns with
    | [ connA; connB ] -> Lwt.return (connA, connB)
    | _ -> failwith "Server.connect_to_clients returned an invalid number of connections"
end

(** A logger to that prints the messages to stdout. *)
module StdoutLogger : Engine.Logger = struct
  let log fmt = Format.ksprintf (fun str -> Lwt_io.printf "[info] %s\n" str) fmt
end

(** A logger that ignores messages. *)
module SilentLogger : Engine.Logger = struct
  let log fmt = Format.ksprintf (fun _ -> Lwt.return ()) fmt
end

let () =
  (* Parse the command line options. *)
  Arg.parse cli_options (fun _ -> Arg.usage cli_options usage_msg) usage_msg;
  (* Instantiate the engine. *)
  let module E =
    (val if !verbose
         then (module Engine.Make (StdoutLogger) (Conn) : Engine.Sig)
         else (module Engine.Make (SilentLogger) (Conn) : Engine.Sig))
  in
  (* Play the game. *)
  Lwt_main.run @@ E.play ()
