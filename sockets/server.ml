open Lwt.Syntax

(** One side of a connection : we can send and receive data. *)
type connection =
  { socket : Lwt_unix.file_descr; in_chan : Lwt_io.input_channel; out_chan : Lwt_io.output_channel }

exception Connection_closed

(** Create a server-side socket. *)
let create_socket addr port =
  (* Create a socket with an IPV6 address. *)
  let socket = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  (* Make sure we can reuse the same socket several times. *)
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  (* Forbid using an IPV4 address (IPV6 only). *)
  Lwt_unix.setsockopt socket Unix.IPV6_ONLY true;
  (* Bind the socket to the address and port. *)
  let* () = Lwt_unix.bind socket @@ ADDR_INET (addr, port) in
  (* Start listening on the socket. *)
  let max_pending_requests = 10 in
  Lwt_unix.listen socket max_pending_requests;
  Lwt.return socket

(** Check a client address is authorized to connect to the server.
    We only accept connections comming from the localhost (this machine), in IPV6. *)
let client_authorized client_addr : bool =
  match client_addr with
  | Unix.ADDR_INET (inet_addr, _port) -> inet_addr = Unix.inet6_addr_loopback
  | Unix.ADDR_UNIX _ -> false

(** Turn a socket address into a string. *)
let string_of_socketaddr addr : string =
  match addr with
  | Unix.ADDR_INET (inet_addr, port) ->
      Format.sprintf "%s/%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX file -> file

(** Accept exactly [n] connections on [socket], and return the corresponding connections. *)
let rec accept_connections socket n : connection list Lwt.t =
  if n <= 0
  then Lwt.return []
  else
    (* Accept a single connection request. *)
    let* fd, client_addr = Lwt_unix.accept socket in
    if client_authorized client_addr
    then
      (* We received a valid connection request. *)
      let in_chan = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
      let out_chan = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      let conn = { socket; in_chan; out_chan } in
      let* () =
        Lwt_io.printf "Accepted connection request from %s\n" (string_of_socketaddr client_addr)
      in
      (* Accept the remaining connections. *)
      let* connections = accept_connections socket (n - 1) in
      Lwt.return @@ (conn :: connections)
    else
      (* The client was not authorized to connect. *)
      let* () =
        Lwt_io.printf "REFUSED connection request from %s\n" (string_of_socketaddr client_addr)
      in
      accept_connections socket n

let connect_to_clients ?(addr = Unix.inet6_addr_loopback) ?(port = 8000) count :
    connection list Lwt.t =
  let* socket = create_socket addr port in
  accept_connections socket count

let receive_request (conn : connection) : Protocol.request Lwt.t =
  (* Wait for the request. *)
  let* req_str =
    try%lwt Lwt_io.read_line_opt conn.in_chan
    with Unix.Unix_error (Unix.ECONNRESET, _, _) -> raise Connection_closed in
  (* Decode the request. *)
  match req_str with
  | None ->
      (* The connection was closed by the client. *)
      raise Connection_closed
  | Some req_str ->
      let req = req_str |> Yojson.Basic.from_string |> Protocol.decode_request in
      Lwt.return req

let send_response (conn : connection) (resp : Protocol.response) : unit Lwt.t =
  (* Encode the response. *)
  let resp_str = resp |> Protocol.encode_response |> Yojson.Basic.to_string in
  (* Send the response. *)
  Lwt_io.write_line conn.out_chan resp_str

let close (conn : connection) : unit = Lwt_unix.shutdown conn.socket SHUTDOWN_ALL
