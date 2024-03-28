open Lwt.Syntax

(** One side of a connection : we can send and receive data. *)
type connection = Lwt_io.input_channel * Lwt_io.output_channel

(*let send_response conn (resp : Protocol.response) : unit Lwt.t = _
  let read_request conn : Protocol.request option Lwt.t = _*)

(** Create a client-side socket and connect to the server. *)
let connect addr port : connection Lwt.t =
  (* Create a socket with an IPV6 address. *)
  let socket = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  (* Make sure we can reuse the same socket several times. *)
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  (* Connect to the server. *)
  let* () = Lwt_unix.connect socket (ADDR_INET (addr, port)) in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  Lwt.return (ic, oc)

(** A client that connects to the server. *)
let create_client ?(addr = Unix.inet6_addr_loopback) ?(port = 8000) () :
    unit Lwt.t =
  let* ic, oc = connect addr port in
  let* () = Lwt_io.write_line oc "hello from the client" in
  let* msg = Lwt_io.read_line_opt ic in
  match msg with None -> Lwt.return () | Some msg -> Lwt_io.printl msg

let () = Lwt_main.run (create_client ())
