open Cohttp
open Cohttp_lwt_unix
open Lwt.Infix

let make_get_request url =
  Lwt.catch
    (fun () ->
      Client.get (Uri.of_string url) >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Cohttp_lwt.Body.to_string body >|= fun body -> Ok (code, body))
    (fun ex -> Lwt.return (Error ex))
(* Return None if an exception occurs *)

let make_patch_request url body =
  Lwt.catch
    (fun () ->
      Client.patch ~body:(Cohttp_lwt.Body.of_string body) (Uri.of_string url)
      >>= fun (_, _) -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Error ex))

let make_delete_request url =
  Lwt.catch
    (fun () ->
      Client.delete (Uri.of_string url) >>= fun (_, _) -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Error ex))

let rec poll_until_ready url delay =
  Lwt_unix.sleep delay >>= fun () ->
  Lwt_io.printf "Waiting for node to fully start: %s\n" url >>= fun _ ->
  make_get_request url >>= function
  | Ok (code, body) ->
      if code = 200 && body <> "" then Lwt.return_unit
      else poll_until_ready url delay
  | Error _ -> poll_until_ready url delay (* Retry if an error occurs *)

let fetch_json_type url encoding =
  let open Data_encoding in
  let rec try_loop tries last_error =
    if tries < 10 then
      let%lwt request_result = make_get_request url in
      match request_result with
      | Error e -> try_loop (succ tries) e
      | Ok (200, body) -> (
          match Json.from_string body with
          | Error e -> try_loop (succ tries) (Failure e)
          | Ok a -> (
              try
                let response = Json.destruct encoding a in
                Lwt.return_ok response
              with exn ->
                Logger.return_error_and_log
                  (Failure
                     (Printf.sprintf
                        "Exception %s with body %s"
                        (Printexc.to_string exn)
                        body))))
      | Ok (v, _) ->
          try_loop
            (succ tries)
            (Failure (Printf.sprintf "Invalid Code %d on url %s" v url))
    else Logger.return_error_and_log last_error
  in
  try_loop 0 (Failure "NONE")
