open Lwt.Infix


let rec random_send oc =
  let delay = (Random.int 16 + 5) |> float_of_int in
  Lwt_unix.sleep delay >>= fun () ->
  let message = "Random message" in
  Lwt_io.write_line oc message >>= fun () ->
  random_send oc

let random_drop ic oc =
  let delay = (Random.int 61 + 60) |> float_of_int in
  Lwt_unix.sleep delay >>= fun () ->
  Lwt_io.close ic >>= fun () ->
  Lwt_io.close oc

let attach ic oc =
  Lwt.async (fun () -> random_send oc);
  Lwt.async (fun () -> random_drop ic oc)
