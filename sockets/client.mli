(** The abstract type of a (socket) connection to the server. *)
type connection

exception Connection_failed
exception Connection_closed

(** Create a connection to the server. 
    By default the server address is localhost (this machine). 
    Raises [Connection_failed] if the connection failed for any reason. *)
val connect : ?addr:Unix.inet_addr -> ?port:int -> unit -> connection Lwt.t

(** Send a request to the server, and wait for a response. 
    Raises [Connection_closed] if the server closed the connection. *)
val send_request : connection -> Protocol.request -> Protocol.response Lwt.t

(** Close a connection. *)
val close : connection -> unit
