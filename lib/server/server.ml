open Lwt.Infix
open Lwt_unix
open Common.Client_pool.ClientPool


let send_message oc message =
  Lwt_io.write_line oc message

let send_message_all message =
  let clients = all_clients () in

  let rec send_to_all = function
    | [] -> Lwt.return_unit
    | client :: rest ->
      send_message client.oc message >>= fun () ->
      send_to_all rest
  in
  send_to_all clients

let set_keepalive socket =
  setsockopt socket SO_KEEPALIVE true;
  Lwt.return_unit

let start address port =
  let sockaddr = Unix.ADDR_INET (address, port) in
  let callback _client_address client_socket =
    set_keepalive client_socket >>= fun () ->
    Lwt.return_unit
  in
  Logs_lwt.info (fun m -> m "Starting server on ::%d" port) >>= fun () ->
  Lwt_io.establish_server_with_client_socket sockaddr callback >>= fun _server ->
  Lwt.return_unit
