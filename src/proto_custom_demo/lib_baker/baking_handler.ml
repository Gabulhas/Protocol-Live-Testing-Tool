let start_bakery (cctxt : Protocol_client_context.full) account () =
  let open Bake_state in
  let create_operation_monitoring_state () =
    let operation_pool = Operation_set.empty in
    let canceler = Lwt_canceler.create () in
    let lock = Lwt_mutex.create () in
    {operation_pool; canceler; lock}
  in
  let monitoring_state = create_operation_monitoring_state () in
  cctxt#message "Bakery started! " >>= fun () ->
  Lwt.async (fun () ->
      Operation_handler.operation_worker cctxt monitoring_state) ;
  Mining.mine_worker cctxt monitoring_state account ()

let baking_schedule = start_bakery
