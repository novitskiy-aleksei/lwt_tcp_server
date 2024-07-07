val keep : int -> Client.connection_config -> unit Lwt.t
(** Keeps a specified number of client connections alive by continually monitoring and
    respawning them as they disconnect.
    @param amount The number of clients to keep alive.
    @param config The connection configuration to use for each client.
    @return A unit Lwt promise that resolves when all client maintenance operations are set up. *)
