open Lwt.Infix
open Common.Client_pool.ClientPool


let spawn config =
  Client.connect config >>= fun (ic, oc) ->
  add_client ic oc >>= fun client ->
  Logs_lwt.info (fun m -> m "[SpawnManager] New client %s spawned" (Uuidm.to_string client.id)) >>= fun () ->
  Lwt.return client

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
      | None -> remove_client client)
    (fun _ex -> remove_client client)

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
  Lwt.return (clients_count ()) >>= fun current ->
  if current < amount then
    let amount_to_spawn = amount - current in
    spawn_amount amount_to_spawn config
  else
    Lwt.return_unit

let rec monitor_clients amount config =
  handle_disconnections () >>= fun () ->
  adjust_connected_clients amount config >>= fun () ->
  Lwt_unix.sleep 5.0 >>= fun () ->
  monitor_clients amount config

let keep amount config =
  spawn_amount amount config >>= fun () ->
  monitor_clients amount config