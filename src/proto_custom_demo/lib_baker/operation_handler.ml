open Bake_state

let monitor_operations cctxt =
  let open Protocol_client_context in
  Alpha_block_services.Mempool.monitor_operations
    cctxt
    ~chain:cctxt#chain
    ~applied:true
    ~branch_delayed:true
    ~branch_refused:false
    ~refused:false
    ()
  >>=? fun (operation_stream, stream_stopper) ->
  let operation_stream =
    Lwt_stream.map
      (fun ops -> List.map (fun ((_, op), _) -> op) ops)
      operation_stream
  in
  Shell_services.Blocks.Header.shell_header
    cctxt
    ~chain:cctxt#chain
    ~block:(`Head 0)
    ()
  >>=? fun shell_header ->
  return (shell_header.level, operation_stream, stream_stopper)

(*So far theres only a type of transactions*)
let get_latest_operations state =
  Lwt.return [Operation_set.elements state.operation_pool]

let operation_worker cctxt state =
  let open Protocol_client_context in
  let rec worker_loop () =
    monitor_operations cctxt >>= function
    | Error err ->
        Format.eprintf
          "@[<hv 2>Failed to send event:@ %a@]@."
          Error_monad.pp_print_trace
          err ;
        Lwt.return_unit
    | Ok (_, operation_stream, op_stream_stopper) ->
        Alpha_block_services.Mempool.request_operations cctxt () >>= fun _ ->
        state.canceler <- Lwt_canceler.create () ;
        Lwt_canceler.on_cancel state.canceler (fun () ->
            op_stream_stopper () ;
            Lwt.return_unit) ;

        state.operation_pool <- Operation_set.empty ;
        let rec loop () =
          Lwt_stream.get operation_stream >>= function
          | None ->
              op_stream_stopper () ;
              worker_loop ()
          | Some ops ->
              (* Define operation_to_string as per your needs *)
              state.operation_pool <-
                List.fold_left
                  (fun acc op -> Operation_set.add op acc)
                  state.operation_pool
                  ops ;
              Lwt_io.printf
                "Pool size %d"
                (Operation_set.elements state.operation_pool |> List.length)
              >>= fun () -> loop ()
        in
        loop ()
  in
  worker_loop ()
