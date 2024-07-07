open Lwt.Infix
open Common.Client_pool.ClientPool


let listen_address = Unix.inet_addr_any
let listen_port = 54321

let main () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);

  let clients_number = Main_extensions.get_clients_number () in
  set_pool_size clients_number;

  let server_task =
    Logs_lwt.info (fun m -> m "[Main] Starting server") >>= fun () ->
    Server.start listen_address listen_port >>= fun () ->
    Logs_lwt.info (fun m -> m "[Main] Server started") in

  (* Запуск менеджера *)
  let manager_task =
    Logs_lwt.info (fun m -> m "[Main] Starting manager") >>= fun () ->
    Spawn_manager.keep clients_number { address = listen_address; port = listen_port } >>= fun () ->
    Logs_lwt.info (fun m -> m "[Main] Manager started") in

  (* Запуск обробки консольних повідомлень *)
  let console_task =
    Logs_lwt.info (fun m -> m "[Main] Starting listener") >>= fun () ->
    Main_extensions.listen_console_messages (fun message -> Server.send_message_all message) >>= fun () ->
    Logs_lwt.info (fun m -> m "[Main] Listener started") in

  (* Виконання всіх завдань паралельно *)
  Lwt.join [
    server_task;
    manager_task;
    console_task
  ]

let () =
  Lwt_main.run (main ())
