open Lwt.Infix
open Lwt_unix


type connection_config = {
  address: Unix.inet_addr;
  port: int;
}

let send_message sock message =
  Logs_lwt.info (fun m -> m "[Client] Send message '%s'" message) >>= fun () ->
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output sock in
  Lwt_io.fprintl oc message

let disconnect sock =
  Lwt_unix.close sock  >>= fun () ->
  Logs_lwt.info (fun m -> m "[Client] Disconnecting client")

let send_ack sock msg =
  send_message sock ("Ack: " ^ string_of_int (String.length msg))

let rec receive_messages sock =
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input sock in
  if Lwt_unix.state sock = Lwt_unix.Closed then
    Logs_lwt.info (fun m -> m "[Client] Socket is already closed") >>= fun () ->
    Lwt.return ()
  else
    Lwt_io.read_line_opt ic >>= function
    | Some msg ->
      send_ack sock msg >>= fun () ->
      receive_messages sock
    | None ->
      Logs_lwt.info (fun m -> m "[Client] Connection closed by server") >>= fun () ->
      Lwt.return ()

let send_keep_alive sock =
  let rec loop () =
    Lwt_unix.sleep (float_of_int (Random.int 15 + 0)) >>= fun () ->
    if Lwt_unix.state sock <> Lwt_unix.Closed then
      send_message sock "я живий" >>= fun () ->
      loop ()
    else
      Lwt.return ()
  in
  loop ()

let simulate_client_life sock =
  Lwt_unix.sleep (float_of_int (Random.int 10 + 0)) >>= fun () ->
  disconnect sock

let connect_to_server address port =
  let sock = socket PF_INET SOCK_STREAM 0 in
  setsockopt sock SO_KEEPALIVE true;
  let sockaddr = ADDR_INET (address, port) in
  Lwt_unix.connect sock sockaddr >|= fun () ->
  sock

let connect (config : connection_config) =
  let%lwt socket = connect_to_server config.address config.port in

  Logs_lwt.info (fun m -> m "[Client] Connected to server") >>= fun () ->

  Lwt.join [
    receive_messages socket;
    send_keep_alive socket;
    simulate_client_life socket;
  ]
