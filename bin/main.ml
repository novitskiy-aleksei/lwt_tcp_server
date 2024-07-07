open Common.Client_pool.ClientPool


let listen_address = Unix.inet_addr_any
let listen_port = 54321


let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Debug) in

  let clients_number = Main_extensions.get_clients_number () in
  set_pool_size clients_number;

  Lwt_main.run (
    let%lwt () = Server.start listen_address listen_port in
    let%lwt () = Spawn_manager.keep clients_number { Client.address = listen_address; port = listen_port } in
    let%lwt () = Main_extensions.listen_console_messages (fun message -> Server.send_message_all message) in
    Lwt.return_unit
  )
