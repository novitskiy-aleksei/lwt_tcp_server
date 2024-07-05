open Lwt.Infix
open Lwt_io

type client = {
  id: int;
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
}

let clients : client list ref = ref []

let add_client (client : client) =
  clients := client :: !clients

let remove_client (client : client) =
  clients := List.filter (fun c -> c.id != client.id) !clients

let send_message message =
  let rec send_to_all = function
    | [] -> Lwt.return_unit
    | client :: rest ->
      Lwt_io.write_line client.oc message >>= fun () ->
      send_to_all rest
  in
  send_to_all !clients

let handle_client (client : client) =
  let rec loop () =
    read_line_opt client.ic >>= function
    | Some line ->
      write_line client.oc ("Echo: " ^ line) >>= loop
    | None -> Lwt.return_unit
  in
  loop () >>= fun () ->
  remove_client client;
  Lwt_io.close client.ic >>= fun () ->
  Lwt_io.close client.oc

let set_keepalive socket =
  let open Unix in
  setsockopt socket SO_KEEPALIVE 1;
  Lwt.return_unit

let start address port =
  let sockaddr = Unix.ADDR_INET (address, port) in
  let callback _client_address client_socket =
    let ic = Lwt_io.of_fd ~mode:Input client_socket in
    let oc = Lwt_io.of_fd ~mode:Output client_socket in
    set_keepalive (Lwt_unix.unix_file_descr client_socket) >>= fun () ->
    let client = { id = Unix.getpid (); ic; oc } in
    add_client client;
    handle_client client
  in
  Lwt_io.printf "Starting server on ::%d\n" port >>= fun () ->
  Lwt_io.establish_server_with_client_socket sockaddr callback >>= fun _server ->
  Lwt.return_unit
