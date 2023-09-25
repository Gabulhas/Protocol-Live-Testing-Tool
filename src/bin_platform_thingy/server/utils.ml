open Lwt.Infix

let show_command_outputs = true

let construct_json_to_string encoding v =
  Data_encoding.Json.construct encoding v |> Data_encoding.Json.to_string

let time_to_string time =
  Printf.sprintf
    "%02d-%02d-%02d"
    time.Unix.tm_mday
    time.Unix.tm_hour
    time.Unix.tm_min

let result_to_json_string = function
  | Error e -> Printf.sprintf "{\"error\": \"%s\"}" e
  | Ok s -> Printf.sprintf "{\"success\": \"%s\"}" s

let contains_substring str sub =
  try
    ignore (Str.search_forward (Str.regexp_string sub) str 0) ;
    true
  with Not_found -> false

let exec_command ?(wait = false) cmd args =
  let process = Lwt_process.open_process ("", Array.of_list (cmd :: args)) in

  (if show_command_outputs then Lwt.return_unit
  else Lwt_io.close process#stdout)
  >>= fun () ->
  if wait then
    let%lwt _ = process#status in
    Lwt.return process
  else Lwt.return process

let path_of_list l = String.concat "/" l

let ensure_dir_exists dir =
  if not (Sys.file_exists dir) then Lwt_unix.mkdir dir 0o755
  else Lwt.return_unit

let timestamp_as_seconds () =
  let timestamp = Unix.time () in
  Int64.of_float (floor timestamp)

let strip_quotes s =
  if String.length s >= 2 && s.[0] = '"' && s.[String.length s - 1] = '"' then
    String.sub s 1 (String.length s - 2)
  else s
