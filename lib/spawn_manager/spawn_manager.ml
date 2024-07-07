open Lwt.Infix
open Common.Client_pool.ClientPool


let spawn config =
  Client.connect config >>= fun (ic, oc) ->
  let client = add_client ic oc in
  Logs_lwt.info (fun m -> m "[SpawnManager] Client %d spawned" client.id) >>= fun () ->
  Lwt.return client

let close client =
  Logs_lwt.info (fun m -> m "[SpawnManager] Client %d to be disconnected" client.id) >>= fun () ->
  remove_client client;
  Lwt_io.close client.ic >>= fun () ->
  Lwt_io.close client.oc >>= fun () ->
  Logs_lwt.info (fun m -> m "[SpawnManager] Client %d disconnected" client.id)

let spawn_amount amount config =
  Logs_lwt.info (fun m -> m "[SpawnManager] Request to spawn %d clients" amount) >>= fun () ->
  let rec loop i =
    if i >= amount then
      Lwt.return_unit
    else
      spawn config >>= fun _client ->
      loop (i + 1)
  in
  loop 0

let disconnect_handler client =
  Lwt.catch
    (fun () ->
      Lwt_io.read_line_opt client.ic >>= function
      | Some _line -> Lwt.return_unit
      | None -> close client
    )
    (fun _ex -> close client)

let handle_disconnections () =
  let clients = all_clients () in
  let rec iter_clients = function
    | [] -> Lwt.return_unit
    | client :: rest ->
      disconnect_handler client >>= fun () ->
      iter_clients rest
  in
  iter_clients clients

let adjust_connected_clients amount config =
  let current_clients = clients_count () in
  if current_clients < amount then
    let amount_to_spawn = amount - current_clients in
    spawn_amount amount_to_spawn config
  else
    Lwt.return_unit

let rec monitor_clients amount config =
  handle_disconnections () >>= fun () ->
  Lwt_unix.sleep 5.0 >>= fun () ->
  adjust_connected_clients amount config >>= fun () ->
  monitor_clients amount config

let keep amount config =
  Lwt.join [
    spawn_amount amount config;
    monitor_clients amount config
  ]
