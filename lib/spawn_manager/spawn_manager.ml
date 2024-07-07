open Lwt.Infix


let spawn_client config =
  Lwt.catch
    (fun () ->
      Logs_lwt.info (fun m -> m "[SpawnManager] Spawning a new client") >>= fun () ->
      Client.connect config
    )
    (fun e ->
      Logs_lwt.warn (fun m -> m "[SpawnManager] Error during client operation: %s" (Printexc.to_string e)) >>= fun () ->
      Lwt.return_unit
    )

let rec monitor_and_respawn config client_promise =
  client_promise >>= fun () ->
  Logs_lwt.info (fun m -> m "[SpawnManager] Client disconnected, spawning a new client") >>= fun () ->
  let new_client = spawn_client config in
  monitor_and_respawn config new_client

let keep amount config =
  Logs_lwt.info (fun m -> m "[SpawnManager] Starting to keep %d clients" amount) >>= fun () ->
  let clients = List.init amount (fun _ -> spawn_client config) in
  Lwt.join (List.map (monitor_and_respawn config) clients)
