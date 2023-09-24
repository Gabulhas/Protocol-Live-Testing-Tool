open Utils

let log_dir = "./logs_platform_thing"

type logger_header = {
  program_start : string;
  test_start : string;
  protocol : string;
  nodes : int32;
  parameters : Data_encoding.Json.t;
}
[@@deriving encoding, show]

type log_level = HEADER | INFO | NODE | CLIENT | DEV | ERROR

let level_str = function
  | HEADER -> "HEADER"
  | INFO -> "INFO"
  | NODE -> "NODE"
  | CLIENT -> "CLIENT"
  | ERROR -> "ERROR"
  | DEV -> "DEV"

let log_file = ref None

let log level message =
  let timestamp = Unix.gettimeofday () in
  let tm = Unix.gmtime timestamp in
  let time_str =
    Printf.sprintf
      "%02d-%02d %02d:%02d:%02d UTC"
      (tm.Unix.tm_mon + 1)
      tm.Unix.tm_mday
      tm.Unix.tm_hour
      tm.Unix.tm_min
      tm.Unix.tm_sec
  in
  match !log_file with
  | Some chan ->
      Lwt_io.fprintf chan "[%s][%s]: %s\n%!" time_str (level_str level) message
  | None -> Lwt.fail (Failure "Log file not initialized")

let start_new_logger logger_header_start =
  let open Lwt.Infix in
  let filename =
    Filename.concat log_dir
    @@ Printf.sprintf
         "log_start_%s_current_%s.log"
         logger_header_start.program_start
         logger_header_start.test_start
  in
  ensure_dir_exists log_dir >>= fun _ ->
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

  log HEADER logger_as_string

let return_error_and_log e =
  let%lwt _ = log ERROR (Printexc.to_string e) in
  Lwt.return_error e
