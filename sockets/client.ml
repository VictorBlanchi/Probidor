open Lwt.Syntax

(** One side of a connection : we can send and receive data. *)
type connection = Lwt_io.input_channel * Lwt_io.output_channel

exception Connection_failed
exception Connection_closed

(** Create a client-side socket and connect to the server. *)
let connect ?(addr = Unix.inet6_addr_loopback) ?(port = 8000) () :
    connection Lwt.t =
  (* Create a socket with an IPV6 address. *)
  let socket = Lwt_unix.socket PF_INET6 SOCK_STREAM 0 in
  (* Make sure we can reuse the same socket several times. *)
  Lwt_unix.setsockopt socket Unix.SO_REUSEADDR true;
  (* Forbid using an IPV4 address (IPV6 only). *)
  Lwt_unix.setsockopt socket Unix.IPV6_ONLY true;
  (* Connect to the server. *)
  let* () =
    try%lwt Lwt_unix.connect socket (ADDR_INET (addr, port))
    with Unix.Unix_error _ -> raise Connection_failed
  in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  Lwt.return (ic, oc)

(** A client that connects to the server. *)
let send_request (conn : connection) (req : Protocol.request) :
    Protocol.response Lwt.t =
  let ic, oc = conn in
  (* Encode the request. *)
  let req_str = req |> Protocol.encode_request |> Yojson.Basic.to_string in
  (* Send the request. *)
  let* () = Lwt_io.write_line oc req_str in
  (* Wait for the response. *)
  let* resp_str =
    try%lwt Lwt_io.read_line_opt ic
    with Unix.Unix_error (Unix.ECONNRESET, _, _) -> raise Connection_closed
  in
  (* Decode the response. *)
  match resp_str with
  | None ->
      (* The connection was closed by the server. *)
      raise Connection_closed
  | Some resp_str ->
      let resp =
        resp_str |> Yojson.Basic.from_string |> Protocol.decode_response
      in
      Lwt.return resp
