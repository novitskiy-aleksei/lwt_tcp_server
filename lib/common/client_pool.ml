module type ClientPoolSig = sig
  type client = {
    id: int;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  val set_pool_size : int -> unit
  val add_client : Lwt_io.input_channel -> Lwt_io.output_channel -> client
  val remove_client : client -> unit
  val all_clients : unit -> client list
  val clients_count : unit -> int
end

module ClientPool : ClientPoolSig = struct
  type client = {
    id: int;
    ic: Lwt_io.input_channel;
    oc: Lwt_io.output_channel;
  }

  let clients : (int, client) Hashtbl.t ref = ref (Hashtbl.create 16)
  let next_id = ref 0

  let generate_id () =
    let id = !next_id in
    incr next_id;
    id

  let set_pool_size size =
    clients := Hashtbl.create size

  let add_client ic oc =
    let id = generate_id () in
    let client = { id; ic; oc } in
    Hashtbl.replace !clients id client;
    client

  let remove_client (client : client) =
    Hashtbl.remove !clients client.id

  let all_clients () =
    Hashtbl.fold (fun _ client acc -> client :: acc) !clients []

  let clients_count () =
    Hashtbl.length !clients
end
