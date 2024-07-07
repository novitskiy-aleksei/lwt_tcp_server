open Lwt.Infix
open Lwt_unix


type connection_config = {
  address: Unix.inet_addr;
  port: int;
}

let set_keepalive socket =
  Lwt_unix.setsockopt socket SO_KEEPALIVE true;
  Lwt.return_unit

let send_message oc message =
  Logs_lwt.info (fun m -> m "[Client] Send message '%s'" message) >>= fun () ->
  Lwt_io.write_line oc message >>= fun () ->
  Logs_lwt.info (fun m -> m "[Client] Message '%s' sent successfully" message)

let acknowledge_message oc =
  send_message oc "OK"

let disconnect ic oc =
  Logs_lwt.info (fun m -> m "[Client] Disconnecting client") >>= fun () ->
  Lwt_io.close ic >>= fun () ->
  if not (Lwt_io.is_closed oc) then
    Lwt_io.close oc
  else
    Lwt.return_unit

let connect (config : connection_config) =
  let sockaddr = Unix.ADDR_INET (config.address, config.port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Lwt_unix.connect socket sockaddr >>= fun () ->
  set_keepalive socket >>= fun () ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in

  Logs_lwt.info (fun m -> m "[Client] Connected to server") >>= fun () ->

(*  let bound_send_message = send_message oc in *)
(*  let bound_disconnect = fun () -> disconnect ic oc in *)
(*  *)
(*  Random_behavior.attach bound_send_message bound_disconnect; *)

  Random_behavior.attach ic oc;
  Lwt.return (ic, oc)
