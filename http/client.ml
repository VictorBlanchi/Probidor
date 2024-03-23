open Cohttp
open Cohttp_lwt_unix
open Protocol


(** Get the URI of the server. *)
let server_uri ?(port=8000) () : Uri.t =
  Uri.of_string (Format.sprintf "http://localhost:%d/" port)

(** Make a single http request to the server. 
    Run this using Lwt_main.run. *)
let make_request (uri : Uri.t) (req : request) : response Lwt.t =
  let open Lwt.Syntax in
  (* Encode the request. *)
  let req_body = req |> encode_request |> Yojson.Basic.to_string |> Cohttp_lwt.Body.of_string in
  (* Send the request. *)
  let* resp, resp_body = Client.post uri ~body:req_body in
  (* Decode the response. *)
  let resp_code = resp |> Response.status in
  let* resp_body = resp_body |> Cohttp_lwt.Body.to_string in
  let resp = 
    try 
      decode_response resp_code (Yojson.Basic.from_string resp_body)
    with 
      Yojson.Basic.Util.Type_error (msg, json) -> 
        (* We couldn't decode the server response : raise an exception. *)
        let err = 
          Format.sprintf 
            "Failed to decode server response.\nResponse code: %d\nResponse body:\n%s\nError: %s\n%s\n" 
            (Code.code_of_status resp_code)
            resp_body
            msg 
            (Yojson.Basic.pretty_to_string json) 
        in raise (Failure err)
  in Lwt.return resp
    