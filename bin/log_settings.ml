let setup file_path =
  let log_out = open_out file_path in
  let log_formatter = Format.formatter_of_out_channel log_out in
  let reporter = Logs_fmt.reporter ~app:log_formatter () in
  Logs.set_reporter reporter;
  Logs.set_level (Some Logs.Info);
  at_exit (fun () -> close_out log_out)
