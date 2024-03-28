open Lwt.Syntax

(** One side of a connection : we can send and receive data. *)
type connection = Lwt_io.input_channel * Lwt_io.output_channel

(*let send_response conn (resp : Protocol.response) : unit Lwt.t = _
  let read_request conn : Protocol.request option Lwt.t = _*)

(** Create a server-side socket. *)
let create_socket addr port =
  (* Create a socket with an IPV6 address. *)
  let socket = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  (* Make sure we can reuse the same socket several times. *)
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  (* Bind the socket to an IPV6 address. *)
  let* () = Lwt_unix.bind socket @@ ADDR_INET (addr, port) in
  (* Start listening on the socket. *)
  Lwt_unix.listen socket 10;
  Lwt.return socket

(** Handle a connection. *)
let handle_connections (conns : connection list) : unit Lwt.t =
  let* () = Lwt_io.printl "Connection started." in
  Lwt_list.iter_p
    begin
      fun (ic, oc) ->
        let* msg = Lwt_io.read_line_opt ic in
        match msg with
        | None ->
            let* () = Lwt_io.printl "client connection dropped" in
            Lwt_io.write_line oc "closed"
        | Some msg -> Lwt_io.write_line oc msg
    end
    conns

(** Turn a socket address into a string. *)
let string_of_socketaddr addr : string =
  match addr with
  | Unix.ADDR_INET (inet_addr, port) ->
      Format.sprintf "%s/%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX file -> file

(** Check a client address is authorized to connect to the server.
    We only accept connections comming from the localhost (this machine), in IPV6. *)
let client_authorized client_addr : bool =
  match client_addr with
  | Unix.ADDR_INET (inet_addr, _port) -> inet_addr = Unix.inet6_addr_loopback
  | Unix.ADDR_UNIX _ -> false

(** Accept exactly [n] connections on [socket], and return the corresponding 
    input/output channels. *)
let rec accept_connections socket n : connection list Lwt.t =
  if n <= 0
  then Lwt.return []
  else
    let* fd, client_addr = Lwt_unix.accept socket in
    if client_authorized client_addr
    then
      (* We received a valid connection request. *)
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      let* () =
        Lwt_io.printf "Accepted connection request from %s\n"
          (string_of_socketaddr client_addr)
      in
      (* Accept the remaining connections. *)
      let* connections = accept_connections socket (n - 1) in
      Lwt.return @@ ((ic, oc) :: connections)
    else
      (* The client was not authorized to connect. *)
      let* () =
        Lwt_io.printf "REFUSED connection request from %s\n"
          (string_of_socketaddr client_addr)
      in
      accept_connections socket n

(** A server that accepts a single connection. *)
let create_server ?(addr = Unix.inet6_addr_loopback) ?(port = 8000) () :
    unit Lwt.t =
  let* socket = create_socket addr port in
  let* conns = accept_connections socket 2 in
  handle_connections conns

let () = Lwt_main.run (create_server ())
