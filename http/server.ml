open Cohttp_lwt_unix
open Protocol

(** Handle a request *)
let callback (on_request : request -> response Lwt.t) _conn _req body =
  let open Lwt.Syntax in
  let* request_body = body |> Cohttp_lwt.Body.to_string in
  try
    (* Decode the request. *)
    let request = request_body |> Yojson.Basic.from_string |> decode_request in
    (* Compute the response. *)
    let* response = on_request request in
    (* Encode the response and send it. *)
    let status, body = encode_response response in
    Server.respond_string ~status ~body:(Yojson.Basic.to_string body) ()
  with Yojson.Basic.Util.Type_error (msg, json) ->
    (* We couldn't decode the request : notify the client. *)
    let err =
      Format.sprintf "Failed to decode request.\nError: %s\n%s\n" msg
        (Yojson.Basic.pretty_to_string json)
    in
    let status, body = encode_response (Error err) in
    Server.respond_string ~status ~body:(Yojson.Basic.to_string body) ()

(** Create an HTTP server to interact with clients. *)
let create_server ?(port = 8000) (on_request : request -> response Lwt.t) :
    unit Lwt.t =
  Server.create
    ~mode:(`TCP (`Port port))
    (Server.make ~callback:(callback on_request) ())
