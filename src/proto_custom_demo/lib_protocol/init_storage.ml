(*Assuming that there's no migration, besides from Genesis*)
let prepare_first_block ctxt ~level ~timestamp =
  Raw_context.prepare_first_block ctxt ~level ~timestamp
  >>=? fun (previous_protocol, ctxt) ->
  match previous_protocol with
  | Genesis param ->
      Header_storage.init_current_target_with_target
        ctxt
        param.constants.initial_target
      >>=? fun ctxt ->
      Header_storage.init_epoch_time_with_time ctxt timestamp >>=? fun ctxt ->
      Account_storage.init ctxt

let prepare ctxt ~level ~timestamp =
    Raw_context.prepare ~level ~timestamp ctxt

(*
  Raw_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun ctxt ->
  Storage.Pending_migration_balance_updates.find ctxt
  >>=? function
  | Some balance_updates ->
      Storage.Pending_migration_balance_updates.remove ctxt
      >>= fun ctxt ->
      (* When applying balance updates in a migration, we must attach receipts.
         The balance updates returned from here will be applied in the first
         block of the new protocol. *)
      return (ctxt, balance_updates)
  | None ->
      return (ctxt, [])
*)
