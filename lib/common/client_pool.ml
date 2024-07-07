open Lwt.Infix


module type ClientPoolSig = sig
  type client = {
    id: Uuidm.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  val set_pool_size : int -> unit
  val add_client : Lwt_io.input_channel -> Lwt_io.output_channel -> client Lwt.t
  val remove_client : client -> unit Lwt.t
  val all_clients : unit -> client list
  val clients_count : unit -> int
end

module ClientPool : ClientPoolSig = struct
  type client = {
    id: Uuidm.t;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let clients : (Uuidm.t, client) Hashtbl.t ref = ref (Hashtbl.create 16)

  let generate_id () = Uuidm.v `V4

  let set_pool_size size =
    clients := Hashtbl.create size

  let add_client ic oc =
    let id = generate_id () in
    let client = { id; ic; oc } in
    Hashtbl.replace !clients id client;
    Lwt.return client

  let remove_client (client : client) =
    Lwt_mutex.with_lock (Lwt_mutex.create ()) (fun () ->
      Logs_lwt.info (fun m -> m "[ClientPool] Removing client %s" (Uuidm.to_string client.id)) >>= fun () ->
      Hashtbl.remove !clients client.id;
      Lwt_io.close client.ic >>= fun () ->
      if not (Lwt_io.is_closed client.oc) then
        Lwt_io.close client.oc >>= fun () ->
        Logs_lwt.info (fun m -> m "[ClientPool] Client %s removed and disconnected" (Uuidm.to_string client.id))
      else
        Logs_lwt.info (fun m -> m "[ClientPool] Client %s already disconnected" (Uuidm.to_string client.id))
    )

  let all_clients () =
    Hashtbl.fold (fun _ client acc -> client :: acc) !clients []

  let clients_count () =
    Hashtbl.length !clients
end
