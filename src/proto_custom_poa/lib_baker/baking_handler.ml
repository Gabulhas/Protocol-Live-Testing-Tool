open Protocol.Alpha_context.Account

let start_bakery (cctxt : Protocol_client_context.full) validator =
  let open Bake_state in
  let create_operation_monitoring_state () =
    let operation_pool = Operation_set.empty in
    let canceler = Lwt_canceler.create () in
    let lock = Lwt_mutex.create () in
    {operation_pool; canceler; lock}
  in
  let monitoring_state = create_operation_monitoring_state () in
  cctxt#message "Bakery started! Validating as %s" (to_b58check validator)
  >>= fun () ->
  Lwt.async (fun () ->
      Operation_handler.operation_worker cctxt monitoring_state) ;

  Baking.start_baking_worker cctxt monitoring_state validator
