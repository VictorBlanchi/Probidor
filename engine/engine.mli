(** This module defines the game engine/server. Its functions are to :
    - Handle the connections with the players.
    - Keep track of the game state.
    - Make sure the players don't make an illegal move. 
  Each player communicates only with the server (players don't directly communicate with each other). *)

open Sockets

(** The engine abstracts over the type of connection used to commmunicate with players. 
    This makes it easier to test the engine : we simply provide a dummy implementation of connections,
    that we can manipulate as we wish. *)
module type Connection = sig
  type t

  val connect : unit -> (t * t) Lwt.t
  val send_response : t -> Protocol.response -> unit Lwt.t
  val receive_request : t -> Protocol.request Lwt.t
end

(** The engine also abstract over a logger type. This allows the caller to choose what to do
    with logs : ignore them, store them in a file, print them to stdout, etc. *)
module type Logger = sig
  val log : ('a, unit, string, unit Lwt.t) format4 -> 'a
end

(** This defines the module type for the game engine. *)
module type Sig = sig
  (** Play an entire game. This means : 
      - Creating a new game. 
      - Connecting to the players. 
      - Communicating with the players while to coordinates (and check the legality of) the moves they make.
      - Notify the players when the game is over (someone won). *)
  val play : unit -> unit Lwt.t
end

module Make (L : Logger) (C : Connection) : Sig
