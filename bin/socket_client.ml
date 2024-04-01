open Lwt.Syntax
open Sockets

(** A client that connects to the server. *)
let create_client () : unit Lwt.t =
  let* conn = Client.connect ~addr:Unix.inet6_addr_loopback () in
  Client.close conn;
  let* () = Lwt_io.printl "Shutdown connection" in
  Lwt_unix.sleep 100.

let () = Lwt_main.run (create_client ())
