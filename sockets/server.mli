(** The abstract type of a (socket) connection to a client. *)
type connection

exception Protocol_error of string
exception Connection_closed

(** Listen for connections on the given uri (typically you'll want to use something like http://localhost:8000).
    This returns a stream of connections : trying to get the first 
    element of the stream will wait for a client to connect (as for the next elements).  *)
val listen : Uri.t -> connection Lwt_stream.t Lwt.t

(** Receive a request from a client. 
    Raises [Connection_closed] if the client closed the connection. *)
val receive_request : connection -> Protocol.request Lwt.t

(** Send a response to a client. 
    Raises [Connection_closed] if the client closed the connection. *)
val send_response : connection -> Protocol.response -> unit Lwt.t

(** Close a connection. *)
val close : connection -> unit Lwt.t
