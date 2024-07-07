let listen_address = Unix.inet_addr_any
let listen_port = 54322

let main () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Debug);

  let clients_number = Main_extensions.get_clients_number () in

  let server_task = Server.setup_server listen_address listen_port in
(*  let console_task = Server.read_console () in *)
  let spawn_manager_task = Spawn_manager.keep clients_number { address = listen_address; port = listen_port } in

  Lwt.join [
    server_task;
(*    console_task; *)
    spawn_manager_task
  ]

let () =
  Lwt_main.run (main ())