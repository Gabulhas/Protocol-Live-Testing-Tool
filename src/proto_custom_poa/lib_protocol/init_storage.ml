open Constants_repr

type error += FailedToCreateAccount

let init_and_reward_initial_set ctxt validator_set reward =
  let rec aux current_ctxt l =
    match l with
    | [] -> Lwt.return (Ok current_ctxt)
    | hd :: tl -> (
        Account_storage.create_account current_ctxt hd.address ~balance:reward
        >>= function
        | Ok new_ctxt -> (
            Account_storage.reveal_manager_key new_ctxt hd.address hd.public_key
            >>= function
            | Ok final_ctxt -> aux final_ctxt tl
            | Error _ -> fail FailedToCreateAccount)
        | Error _ -> fail FailedToCreateAccount)
  in
  aux ctxt validator_set

let prepare_first_block ctxt ~level ~timestamp :
    (Raw_context.t, error trace) result Lwt.t =
  Logging.(log Debug "Starting to prepare the first block") ;
  Raw_context.prepare_first_block ctxt ~level ~timestamp
  >>=? fun (previous_protocol, ctxt) ->
  Logging.(log Debug "Prepared First Block on raw_context") ;
  Raw_context.Cache.set_cache_layout ctxt [3] >>= fun ctxt ->
  Logging.(log Debug "Set cache layout") ;
  match previous_protocol with
  | Genesis params ->
      Logging.(log Debug "Handling Genesis block") ;
      Account_storage.init ctxt >>=? fun ctxt ->
      Logging.(log Debug "Initialized Account Storage") ;
      Validator_set_storage.init_with_initial_set
        ctxt
        (List.map (fun v -> v.address) params.constants.initial_validator_set)
      >>=? fun ctxt ->
      Logging.(log Debug "Initialized Validator Set Storage") ;
      init_and_reward_initial_set
        ctxt
        params.constants.initial_validator_set
        params.constants.validator_initial_reward
