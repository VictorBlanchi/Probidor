open Http

let () =
  Server.create_server (fun request ->
      let open Lwt.Syntax in
      let str = request |> Protocol.encode_request |> Yojson.Basic.show in
      let* () = Lwt_io.printl str in
      Lwt.return Protocol.Ok )
  |> Lwt_main.run
