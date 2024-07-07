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
      Logs_lwt.info (fun m -> m "[SpawnManager] Checking client %s" (Uuidm.to_string client.id)) >>= fun () ->
      if Lwt_io.is_closed client.ic then
        remove_client client
      else
        Logs_lwt.info (fun m -> m "[SpawnManager] End Checking client %s" (Uuidm.to_string client.id)) >>= fun () ->
(*        Lwt_io.read_line client.ic >>= fun _ -> *)
        Lwt.return_unit)
    (function
      | End_of_file ->
          Logs_lwt.warn (fun m -> m "[SpawnManager] Client %s disconnected: End_of_file" (Uuidm.to_string client.id)) >>= fun () ->
          remove_client client
      | Unix.Unix_error(Unix.EBADF, _, _) ->
          Logs_lwt.warn (fun m -> m "[SpawnManager] Client %s disconnected: Unix_error EBADF" (Uuidm.to_string client.id)) >>= fun () ->
          remove_client client
      | ex ->
          Logs_lwt.warn (fun m -> m "[SpawnManager] Error in disconnect_handler: %s" (Printexc.to_string ex)) >>= fun () ->
          Logs_lwt.warn (fun m -> m "Backtrace: %s" (Printexc.get_backtrace ())) >>= fun () ->
          remove_client client)

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
  Logs_lwt.info (fun m -> m "[SpawnManager] Monitoring clients started") >>= fun () ->
  Lwt_unix.sleep 5.0 >>= fun () ->
  (* Тут ваш код для моніторингу *)
  Lwt.pause() >>= fun () ->
  monitor_clients amount config
(*  Logs_lwt.info (fun m -> m "[SpawnManager] Monitoring clients started") >>= fun () -> *)
(*  let rec loop () = *)
(*    Logs_lwt.info (fun m -> m "[SpawnManager] Monitor clients %d" (clients_count ())) >>= fun () -> *)
(*    Lwt_unix.sleep 5.0 >>= fun () -> *)
(*    Lwt.pause() >>= fun () -> *)
(*    Logs_lwt.info (fun m -> m "[SpawnManager] Sleeping for 5 seconds") >>= fun () -> *)
(*    Lwt.catch *)
(*      (fun () -> *)
(*        handle_disconnections () >>= fun () -> *)
(*        adjust_connected_clients amount config >>= fun () -> *)
(*        Logs_lwt.info (fun m -> m "[SpawnManager] Woke up, continuing monitoring") >>= fun () -> *)
(*        loop () *)
(*      ) *)
(*      (fun ex -> *)
(*        Logs_lwt.err (fun m -> m "[SpawnManager] Error in monitoring loop: %s" (Printexc.to_string ex)) >>= fun () -> *)
(*        Lwt.return_unit) *)
(*  in *)
(*  loop () *)

let keep amount config =
  Logs_lwt.info (fun m -> m "[SpawnManager] Starting to keep clients") >>= fun () ->
  spawn_amount amount config >>= fun () ->
  Logs_lwt.info (fun m -> m "[SpawnManager] Finished spawning initial clients") >>= fun () ->
  Lwt.async (fun () -> monitor_clients amount config);
  Logs_lwt.info (fun m -> m "[SpawnManager] Started monitoring clients") >>= fun () ->
  Lwt.return_unit
