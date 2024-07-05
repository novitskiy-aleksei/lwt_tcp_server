open Helpers
open Lwt.Infix

let listen_address = Unix.inet_addr_any
let listen_port = 54321

let () =
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in

  Lwt_main.run (
    let%lwt () = Server.start listen_address listen_port in
    let%lwt () = Helpers.listen_console_messages (fun message -> Server.send_message message) in
    let%lwt () = Spawn_manager.keep Helpers.get_clients_number() (listen_address listen_port) in
    Lwt.return_unit
  )
