let listen_address = Unix.inet_addr_any
let listen_port = 54321
let log_file = "tcp_app.log"

let get_clients_number () =
  let args = Sys.argv in
  if Array.length args < 2 then
    failwith "Usage: tcp_server <number_of_processes>"
  else int_of_string args.(1)

let main () =
  Log_settings.setup log_file;

  let server_task = Server.start listen_address listen_port in
  let spawn_manager_task =
    Spawn_manager.keep (get_clients_number ())
      { address = listen_address; port = listen_port }
  in

  Lwt.join [ server_task; spawn_manager_task ]

let () = Lwt_main.run (main ())
