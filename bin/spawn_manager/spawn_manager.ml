open Lwt.Infix


type client = {
  id: int;
  ic: Lwt_io.input_channel;
  oc: Lwt_io.output_channel;
}

let clients : client list ref = ref []


let add_client client =
  clients := client :: !clients

let remove_client client =
  clients := List.filter (fun c -> c != client) !clients

let keep (n, options: Client.connection_config) spawn_client =
  let rec loop i =
    if i > n then
      Lwt.return_unit
    else
      spawn_client i >>= fun () ->
      Lwt_io.printf "Process %d started\n" i >>= fun () ->
      loop (i + 1)
    in
  loop 1

let spawn_client i =
  Lwt_unix.sleep 1.0 >>= fun () ->
  Client.connect config >>= fun (ic, oc) ->
  let client = { id; ic; oc } in
  add_client client;
  Lwt.return_unit
