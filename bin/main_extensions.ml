 open Lwt.Infix


let get_clients_number () =
  let args = Sys.argv in
  if Array.length args < 2
    then failwith "Usage: tcp_server <number_of_processes>"
    else int_of_string args.(1)

let rec listen_console_messages callback =
  Lwt_io.read_line Lwt_io.stdin >>= fun message ->
  callback message >>= fun () ->
  listen_console_messages callback
