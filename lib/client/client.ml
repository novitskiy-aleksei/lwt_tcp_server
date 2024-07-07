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
  Lwt_io.write_line oc message

let acknowledge_message oc =
  Lwt_io.write_line oc "OK"

let connect (config : connection_config) =
  let sockaddr = Unix.ADDR_INET (config.address, config.port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in

  Lwt_unix.connect socket sockaddr >>= fun () ->
  set_keepalive socket >>= fun () ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in

  Random_behavior.attach ic oc;
  Lwt.return (ic, oc)
