val start : Unix.inet_addr -> int -> unit Lwt.t
(** Starts the server, which listens provided address and port.
    Server handles incoming connections and console commands *)

val broadcast : string -> unit Lwt.t
(** Sends messages to all connected clients *)
