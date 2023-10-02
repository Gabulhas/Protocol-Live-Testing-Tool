open Alpha_context

module AccountServices = struct
  let custom_root =
    (RPC_path.(open_root / "context" / "account")
      : RPC_context.t RPC_path.context)

  let get_balance =
    RPC_service.get_service
      ~description:"Get Account Balance"
      ~query:RPC_query.empty
      ~output:Tez_repr.encoding
      RPC_path.(custom_root /: Account_repr.rpc_arg / "balance")

  let exists =
    RPC_service.get_service
      ~description:"Check if account exists"
      ~query:RPC_query.empty
      ~output:Data_encoding.bool
      RPC_path.(custom_root /: Account_repr.rpc_arg / "exists")

  let revealed =
    RPC_service.get_service
      ~description:"Check if account is revealed"
      ~query:RPC_query.empty
      ~output:Data_encoding.bool
      RPC_path.(custom_root /: Account_repr.rpc_arg / "revealed")

  let counter =
    RPC_service.get_service
      ~description:"Get Account counter"
      ~query:RPC_query.empty
      ~output:Data_encoding.z
      RPC_path.(custom_root /: Account_repr.rpc_arg / "counter")

  let register () =
    let open Services_registration in
    register1 ~chunked:false get_balance (fun ctxt key () () ->
        Account_storage.get_balance_exists_account ctxt key >>=? fun balance ->
        return balance) ;

    register1 ~chunked:false exists (fun ctxt key () () ->
        Account_storage.exists ctxt key >>=? fun result -> return result) ;

    register1 ~chunked:false revealed (fun ctxt key () () ->
        Account_storage.is_revealed ctxt key >>=? fun result -> return result) ;

    register1 ~chunked:false counter (fun ctxt key () () ->
        Account_storage.get_counter ctxt key >>=? fun result -> return result)

  module Commands = struct
    let get_balance rpc_ctxt chain_blk account =
      RPC_context.make_call1 get_balance rpc_ctxt chain_blk account () ()

    let revealed rpc_ctxt chain_blk account =
      RPC_context.make_call1 revealed rpc_ctxt chain_blk account () ()

    let exists rpc_ctxt chain_blk account =
      RPC_context.make_call1 exists rpc_ctxt chain_blk account () ()

    let get_counter rpc_ctxt chain_blk account =
      RPC_context.make_call1 counter rpc_ctxt chain_blk account () ()
  end
end

module ConstantServices = struct
  let custom_root =
    (RPC_path.(open_root / "context" / "constants")
      : RPC_context.t RPC_path.context)

  let all =
    RPC_service.get_service
      ~description:"Returns current constants"
      ~query:RPC_query.empty
      ~output:Constants.encoding
      custom_root

  let register () =
    let open Services_registration in
    register0 ~chunked:false all (fun ctxt () () ->
        return @@ Constants.all ctxt)

  module Commands = struct
    let fetch_all_constants rpc_ctxt chain_blk () =
      RPC_context.make_call0 all rpc_ctxt chain_blk () ()
  end
end

module PoAServices = struct
  let custom_root =
    (RPC_path.(open_root / "context" / "poa") : RPC_context.t RPC_path.context)

  let time_rpc_arg =
    RPC_arg.like RPC_arg.int32 ~descr:"A Timestamp in seconds" "timestamp"

  (* Service to return the order of validators at a specific block level *)
  let validator_order_at_level =
    RPC_service.get_service
      ~description:"Order of validators at block level"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Account_repr.encoding)
      RPC_path.(custom_root / "validator_order" /: Raw_level_repr.rpc_arg)

  (* Service to return skips given a timestamp *)
  let skips_at_timestamp =
    RPC_service.post_service
      ~description:
        "Skips at a given levels and previous and current timestamps, assuming \
         that they are within the same round"
      ~query:RPC_query.empty
      ~input:
        (let open Data_encoding in
        obj2
          (req "previous_timestamp" Time.encoding)
          (req "current_timestamp" Time.encoding))
      ~output:Data_encoding.int64
      RPC_path.(custom_root / "skips")

  (* Service to return first validator of next block height *)
  let first_block_validator =
    RPC_service.get_service
      ~description:"First validator of next block height"
      ~query:RPC_query.empty
      ~output:Account_repr.encoding
      RPC_path.(custom_root / "first_block_validator" /: Raw_level_repr.rpc_arg)

  (* Service to return expected validator given level and timestamp *)
  let expected_validator =
    RPC_service.post_service
      ~description:
        "Expected validator at given level and previous_timestamp and \
         current_timestamp"
      ~query:RPC_query.empty
      ~input:
        (let open Data_encoding in
        obj3
          (req "level" int32)
          (req "previous_timestamp" Time.encoding)
          (req "current_timestamp" Time.encoding))
      ~output:(Data_encoding.option Account_repr.encoding)
      RPC_path.(custom_root / "expected_validator")

  let validator_set =
    RPC_service.get_service
      ~description:"List of validators at current context"
      ~query:RPC_query.empty
      ~output:(Data_encoding.list Account_repr.encoding)
      RPC_path.(custom_root / "validators")

  let register () =
    let open Services_registration in
    register1 ~chunked:false validator_order_at_level (fun ctxt level () () ->
        Validator_set_storage.get_validator_set ctxt >|=? fun validators ->
        Round_selection.sort_validators_by_hash_as_addresses validators level) ;
    register0
      ~chunked:false
      skips_at_timestamp
      (fun ctxt () (previous_timestamp, current_timestamp) ->
        let constants = Alpha_context.constants ctxt in
        let tolerance = constants.tolerance |> Time.to_seconds in
        let block_time = constants.block_time |> Time.to_seconds in
        let skips =
          Round_selection.calculate_skips
            (Time.to_seconds current_timestamp)
            (Time.to_seconds previous_timestamp)
            block_time
            tolerance
        in
        Lwt.return (ok skips)) ;
    register0
      ~chunked:false
      expected_validator
      (fun ctxt () (level, previous_timestamp, current_timestamp) ->
        let constants = Alpha_context.constants ctxt in
        let tolerance = constants.tolerance |> Time.to_seconds in
        let block_time = constants.block_time |> Time.to_seconds in
        let round = level in
        Validator_set_storage.get_validator_set ctxt >|=? fun validators ->
        Round_selection.get_validator
          validators
          round
          (Time.to_seconds current_timestamp)
          (Time.to_seconds previous_timestamp)
          block_time
          tolerance) ;

    register0 ~chunked:false validator_set (fun ctxt () () ->
        Validator_set_storage.get_validator_set ctxt)

  module Commands = struct
    let validator_order_at_level rpc_ctxt chain_blk level =
      RPC_context.make_call1
        validator_order_at_level
        rpc_ctxt
        chain_blk
        level
        ()
        ()

    let skips_at_timestamp rpc_ctxt chain_blk previous_timestamp
        current_timestamp =
      RPC_context.make_call0
        skips_at_timestamp
        rpc_ctxt
        chain_blk
        ()
        (previous_timestamp, current_timestamp)

    let first_block_validator rpc_ctxt chain_blk level =
      RPC_context.make_call1
        first_block_validator
        rpc_ctxt
        chain_blk
        level
        ()
        ()

    let expected_validator rpc_ctxt chain_blk level previous_timestamp
        current_timestamp =
      RPC_context.make_call0
        expected_validator
        rpc_ctxt
        chain_blk
        ()
        (level, previous_timestamp, current_timestamp)

    let validator_set rpc_ctxt chain_blk =
      RPC_context.make_call0 validator_set rpc_ctxt chain_blk () ()
  end
end

let register () =
  AccountServices.register () ;
  ConstantServices.register () ;
  PoAServices.register ()
