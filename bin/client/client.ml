 open Lwt.Infix


type connection_config = {
  address: Unix.inet_addr;
  port: int;
}

let set_keepalive socket =
  let open Unix in
  setsockopt socket SO_KEEPALIVE 1;
  (* ... *)
  Lwt.return_unit

let send_message oc message =
  Lwt_io.write_line oc message

let connect (config : connection_config) =
  let sockaddr = Unix.ADDR_INET (config.address, config.port) in
  let socket = Lwt_unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket sockaddr >>= fun () ->
  set_keepalive (Lwt_unix.unix_file_descr socket) >>= fun () ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  Lwt.return (ic, oc)
