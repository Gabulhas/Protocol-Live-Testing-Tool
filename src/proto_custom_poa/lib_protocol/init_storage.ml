let reward_inital_set ctxt validator_set reward =
  let rec aux current_ctxt l =
    match l with
    | [] -> Lwt.return (ok current_ctxt)
    | hd :: tl ->
        Account_storage.create_account ctxt hd ~balance:reward >>=? fun ctxt ->
        aux ctxt tl
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
        params.constants.initial_validator_set
      >>=? fun ctxt ->
      Logging.(log Debug "Initialized Validator Set Storage") ;
      reward_inital_set
        ctxt
        params.constants.initial_validator_set
        params.constants.validator_initial_reward
