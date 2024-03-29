open Lwt.Syntax
open Sockets

let create_server () =
  let* conns = Server.connect_to_clients 1 in
  let conn =
    match conns with
    | [ conn ] -> conn
    | _ -> failwith "wrong number of connections"
  in
  let* () = Lwt_unix.sleep 3. in
  let* () = Server.send_response conn YouWin in
  let* () = Lwt_io.printl "Sent response" in
  let* () = Lwt_unix.sleep 3. in
  Lwt.return ()

let () = Lwt_main.run (create_server ())
