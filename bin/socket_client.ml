open Lwt.Syntax
open Sockets

(** A client that connects to the server. *)
let create_client () : unit Lwt.t =
  let* conn = Client.connect ~addr:Unix.inet6_addr_loopback () in
  (*let* () = Lwt_unix.sleep 10. in*)
  let* resp = Client.send_request conn NewPlayer in
  Lwt_io.printl (resp |> Protocol.encode_response |> Yojson.Basic.to_string)

let () = Lwt_main.run (create_client ())
