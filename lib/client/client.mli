type connection_config = {
  address : Unix.inet_addr; (* The IP address of the server. *)
  port : int; (* The port number on which the server is listening. *)
}
(** The configuration for connecting to a server. *)

val connect : connection_config -> unit Lwt.t
(** Connects to a server using the provided configuration and handles sending and
    receiving messages, as well as simulating client life.
    @param config The connection configuration containing the server address and port.
    @return A unit promise that resolves when the client's interaction completes or the
    connection is terminated. *)
