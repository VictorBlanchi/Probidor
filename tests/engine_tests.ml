open Quoridor
open Sockets

(** Emulate a connection to two players. 
    Players will use a predefined list of requests, and responses from the engine 
    will be stored in two list references. *)
let fake_conn (requestsA : Protocol.request list) (requestsB : Protocol.request list) :
    (module Engine.Connection) * Protocol.response list ref * Protocol.response list ref =
  let requestsA = ref requestsA in
  let requestsB = ref requestsB in
  let receive requests =
    match !requests with
    | [] -> fst @@ Lwt.wait ()
    | req :: requests' ->
        requests := requests';
        Lwt.return req
  in

  let responsesA = ref [] in
  let responsesB = ref [] in
  let send responses resp =
    responses := resp :: !responses;
    Lwt.return ()
  in
  let module C = struct
    type t = State.player

    let connect () = Lwt.return (State.PlayerA, State.PlayerB)

    let receive_request conn =
      match conn with State.PlayerA -> receive requestsA | State.PlayerB -> receive requestsB

    let send_response conn resp =
      match conn with
      | State.PlayerA -> send responsesA resp
      | State.PlayerB -> send responsesB resp
  end in
  ((module C : Engine.Connection), responsesA, responsesB)
