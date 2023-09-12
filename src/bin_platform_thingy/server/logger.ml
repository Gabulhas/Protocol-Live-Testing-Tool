open Utils

type logger_header = {
  program_start : string;
  test_start : string;
  protocol : string;
  nodes : int32;
  parameters : string;
}
[@@deriving encoding, show]

type log_level = HEADER | INFO | NODE | CLIENT | DEV

let level_str = function
  | HEADER -> "HEADER"
  | INFO -> "INFO"
  | NODE -> "NODE"
  | CLIENT -> "CLIENT"
  | DEV -> "DEV"

let log_file = ref None

let async_log level message =
  match !log_file with
  | Some chan -> Lwt_io.fprintf chan "[%s]: %s\n%!" (level_str level) message
  | None -> Lwt.fail (Failure "Log file not initialized")

let start_new_logger logger_header_start =
  let open Lwt.Infix in
  let filename =
    Printf.sprintf
      "log_start_%s_current_%s.log"
      logger_header_start.program_start
      logger_header_start.test_start
  in
  Lwt_io.open_file
    ~flags:[Unix.O_APPEND; Unix.O_CREAT; Unix.O_WRONLY]
    ~perm:0o640
    ~mode:Lwt_io.Output
    filename
  >>= fun new_file ->
  log_file := Some new_file ;

  let logger_as_string =
    construct_json_to_string logger_header_encoding logger_header_start
  in

  async_log HEADER logger_as_string
