(** The abstract type of a (socket) connection to a client. *)
type connection

exception Connection_closed

(** Listen for connections on the given address and port. 
    Accept exactly the given number of connections from clients. *)
val connect_to_clients : addr:Unix.inet_addr -> port:int -> int -> connection list Lwt.t

(** Receive a request from a client. 
    Raises [Connection_closed] if the client closed the connection. *)
val receive_request : connection -> Protocol.request Lwt.t

(** Send a response to a client. 
    Raises [Connection_closed] if the client closed the connection. *)
val send_response : connection -> Protocol.response -> unit Lwt.t

(** Close a connection. *)
val close : connection -> unit
