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
    register1
        ~chunked:false
        get_balance
        (fun ctxt key () ()-> Account_storage.get_balance_exists_account ctxt key >>=? fun balance -> return balance);

    register1
        ~chunked:false
        exists
        (fun ctxt key () ()-> Account_storage.exists ctxt key >>=? fun result -> return result );

    register1
        ~chunked:false
        revealed
        (fun ctxt key () ()-> Account_storage.is_revealed ctxt key >>=? fun result -> return result );

    register1
        ~chunked:false
        counter
        (fun ctxt key () ()-> Account_storage.get_counter ctxt key >>=? fun result -> return result )


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
      register0 ~chunked:false all (fun ctxt () () -> return @@ Constants.all ctxt)

end

module ContextServices = struct
  let custom_root =
  (RPC_path.(open_root / "context" / "other")
    : RPC_context.t RPC_path.context)

  let current_target =
    RPC_service.get_service
      ~description:"Get latest target"
      ~query:RPC_query.empty
      ~output:Target_repr.encoding
      RPC_path.(custom_root /"current_target")


  let current_reward = 
    RPC_service.get_service
      ~description:"Get latest reward"
      ~query:RPC_query.empty
      ~output:Tez_repr.encoding
      RPC_path.(custom_root /"current_reward"/:(RPC_arg.like RPC_arg.int32 ~descr:"A block level" "block_level"))

  let level_and_timestamp =
    let open Data_encoding in
    obj2
      (req "level" int32)
      (req "timestamp" Time.encoding)


  let next_target_post =
    RPC_service.post_service
      ~description:"Get the target given a level and a timestamp"
      ~query:RPC_query.empty
      ~input:level_and_timestamp
      ~output:Target_repr.encoding
      RPC_path.(custom_root /"next_target")


  let register () = 
    let open Services_registration in
    register0 ~chunked:false current_target (fun ctxt () () -> Header_storage.get_current_target ctxt >>=? fun target -> return target);
    register1 ~chunked:false current_reward (fun ctxt level () () -> Apply.calculate_current_reward ctxt level >>=? fun target -> return target);
    register0 ~chunked:false next_target_post (fun ctxt () (level, timestamp) -> Apply.calculate_current_target ctxt level timestamp>>=? fun (target, _) -> return target);

   module Commands = struct
        let current_target rpc_ctxt chain_blk =
            RPC_context.make_call0 current_target rpc_ctxt chain_blk () ()
        let current rpc_ctxt chain_blk level =
            RPC_context.make_call1 current_reward rpc_ctxt chain_blk level () () 
        let next_target rpc_ctxt chain_blk level timestamp =
            RPC_context.make_call0 next_target_post rpc_ctxt chain_blk () (level, timestamp)
    end

end


let register () =
    AccountServices.register ();
    ConstantServices.register ();
    ContextServices.register ()
