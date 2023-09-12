open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix

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

let make_request url =
  Lwt.catch
    (fun () ->
      Client.get (Uri.of_string url) >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Cohttp_lwt.Body.to_string body >|= fun body -> Some (code, body))
    (fun _ex -> Lwt.return None)
(* Return None if an exception occurs *)

let rec poll_until_ready url delay =
  Lwt_unix.sleep delay >>= fun () ->
  Lwt_io.printf "Waiting for node to fully start: %s\n" url >>= fun _ ->
  make_request url >>= function
  | Some (code, body) ->
      if code = 200 && body <> "" then
        Lwt_io.print "FULLY STARTED" >>= fun _ -> Lwt.return_unit
      else poll_until_ready url delay
  | None -> poll_until_ready url delay (* Retry if an error occurs *)
