open Lwt.Infix
open Lwt_unix

let connections = ref []
let unique_id () = Uuidm.v `V4 |> Uuidm.to_string

let add_connection fd =
  let id = unique_id () in
  connections := (id, fd) :: !connections;
  id

let remove_connection id =
  connections := List.filter (fun (id', _) -> id' <> id) !connections

let broadcast message =
  Logs_lwt.info (fun m -> m "[Server] Broadcast message '%s'" message)
  >>= fun () ->
  Lwt_list.iter_p
    (fun (_, fd) ->
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output fd in
      Lwt_io.fprintl oc message)
    !connections

let handle_client fd =
  let id = add_connection fd in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input fd in
  let rec read_loop () =
    Lwt_io.read_line_opt ic >>= function
    | Some message ->
        Logs_lwt.info (fun m -> m "[Server] Received a message '%s'" message)
        >>= fun () -> read_loop ()
    | None ->
        remove_connection id;
        Lwt.return ()
  in
  read_loop ()

let setup_server address port =
  let addr = ADDR_INET (address, port) in
  let sock = socket PF_INET SOCK_STREAM 0 in
  bind sock addr >>= fun () ->
  listen sock 10;
  let rec accept_loop () =
    accept sock >>= fun (client_fd, _) ->
    Lwt.async (fun () -> handle_client client_fd);
    accept_loop ()
  in
  let%lwt () =
    Logs_lwt.info (fun m ->
        m "[Server] Started a server on %s:%d"
          (Unix.string_of_inet_addr address)
          port)
  in
  accept_loop ()

let read_console () =
  let rec loop () =
    Lwt_io.read_line Lwt_io.stdin >>= fun line -> broadcast line >>= loop
  in
  loop ()

let start address port = Lwt.join [ setup_server address port; read_console () ]
