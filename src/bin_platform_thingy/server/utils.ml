open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

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
  if wait then
    let%lwt _ = process#status in
    Lwt.return process
  else Lwt.return process

let make_get_request url =
  Lwt.catch
    (fun () ->
      Client.get (Uri.of_string url) >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Cohttp_lwt.Body.to_string body >|= fun body -> Ok (code, body))
    (fun ex -> Lwt.return (Error ex))
(* Return None if an exception occurs *)

let rec poll_until_ready url delay =
  Lwt_unix.sleep delay >>= fun () ->
  Lwt_io.printf "Waiting for node to fully start: %s\n" url >>= fun _ ->
  make_get_request url >>= function
  | Ok (code, body) ->
      if code = 200 && body <> "" then Lwt.return_unit
      else poll_until_ready url delay
  | Error _ -> poll_until_ready url delay (* Retry if an error occurs *)

let ensure_dir_exists dir =
  if not (Sys.file_exists dir) then Lwt_unix.mkdir dir 0o755
  else Lwt.return_unit

let path_of_list l = String.concat "/" l

let fetch_json_type url encoding =
  let open Data_encoding in
  let%lwt request_result = make_get_request url in
  match request_result with
  | Error e -> Lwt.return_error e
  | Ok (_, body) -> (
      match Json.from_string body with
      | Error e -> Lwt.return_error (Failure e)
      | Ok a ->
          let response = Json.destruct encoding a in
          Lwt.return_ok response)
