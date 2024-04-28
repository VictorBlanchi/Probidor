open Lwt.Syntax
module W = Websocket
module WS = Websocket_lwt_unix

(** One side of a connection : we can send and receive data. *)
type connection = { client : WS.Connected_client.t }

exception Protocol_error of string
exception Connection_closed

(* Codes we send in a [close] frame. The code is used to tell the client
   why we close the connection. The default value for a normal exit is [1000].
   For a list of exit codes see :
   https://developer.mozilla.org/en-US/docs/Web/API/CloseEvent/code *)
let close_normal = 1000
let close_bad_data = 1003

(** Send a close frame.  *)
let send_close ?(code = close_normal) (conn : connection) : unit Lwt.t =
  let frame = W.Frame.close code in
  WS.Connected_client.send conn.client frame

(*(** Wait for a close frame. The boolean indicates whether
      we did really receive a close frame (or something else). *)
  let receive_close (conn : connection) : bool Lwt.t =
    let open W.Frame in
    let* frame = WS.read conn.conn in
    match frame.opcode with Opcode.Close -> Lwt.return true | _ -> Lwt.return false*)

let send_response (conn : connection) (resp : Protocol.response) : unit Lwt.t =
  (* Encode the response. *)
  let resp_str = resp |> Protocol.encode_response |> Yojson.Basic.to_string in
  (* Create the frame. *)
  let frame = W.Frame.create ~opcode:W.Frame.Opcode.Text ~final:true ~content:resp_str () in
  (* Send the frame. *)
  WS.Connected_client.send conn.client frame

(** Receive a fragmented text frame, while handling any
        control frames + errors that might happen. *)
let receive_fragmented (conn : connection) : string Lwt.t =
  let rec loop acc =
    let open W.Frame in
    let* frame = WS.Connected_client.recv conn.client in
    match frame.opcode with
    (* Receive a first text frame. *)
    | Opcode.Text when acc = [] ->
        let acc = [ frame.content ] in
        if frame.final then Lwt.return acc else loop acc
    (* Receive more text frames. *)
    | Opcode.Continuation when acc != [] ->
        let acc = frame.content :: acc in
        if frame.final then Lwt.return acc else loop acc
    (* Ping : answer with Pong. *)
    | Opcode.Ping ->
        let pong = create ~opcode:Opcode.Pong () in
        let* () = WS.Connected_client.send conn.client pong in
        loop acc
    (* Pong : nothing to do. *)
    | Opcode.Pong -> loop acc
    (* Connection closed. *)
    | Opcode.Close ->
        (* Close the connection. *)
        let* () = send_close conn in
        (* Raise an exception. *)
        Lwt.fail @@ Connection_closed
    (* Unsupported opcodes. *)
    | _ ->
        let* () = send_close ~code:close_bad_data conn in
        Lwt.fail
        @@ Protocol_error (Format.sprintf "Unexpected [%s] frame." (Opcode.to_string frame.opcode))
  in
  let* contents = loop [] in
  Lwt.return @@ String.concat "" @@ List.rev contents

let receive_request (conn : connection) : Protocol.request Lwt.t =
  (* Receive a single text message. *)
  let* req_str = receive_fragmented conn in
  (* Decode the request. *)
  let req = req_str |> Yojson.Basic.from_string |> Protocol.decode_request in
  Lwt.return req

let close (conn : connection) : unit Lwt.t = send_close conn

let listen uri =
  (* Create the stream of clients and its push function. *)
  let client_stream, client_push = Lwt_stream.create () in
  (* Handle a single client. *)
  let handler client : unit Lwt.t =
    (* Push the client to the stream. *)
    client_push @@ Some { client };
    (* Wait forever so we don't close the connection. *)
    fst @@ Lwt.wait ()
  in
  (* Setup a server to listen for connections. *)
  let* endpoint = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let* mode = Conduit_lwt_unix.endp_to_server ~ctx endpoint in
  (* Run the server in the background. *)
  Lwt.async (fun () -> WS.establish_server ~ctx ~mode handler);
  (* Return the stream of clients. *)
  Lwt.return client_stream
