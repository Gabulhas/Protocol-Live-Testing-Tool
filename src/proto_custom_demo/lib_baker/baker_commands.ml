let baker_commands () : Protocol_client_context.full Tezos_clic.command list =
  let open Tezos_clic in
  let group =
    {
      Tezos_clic.name = "delegate.baker";
      title = "Commands related to the baker daemon.";
    }
  in
  [
    command
      ~group
      ~desc:"Launch the baker daemon."
      no_options
      (prefixes ["run"]
      @@ Client_proto_args.account_param
           ~name:"account"
           ~desc:"Account b58check"
      @@ Client_proto_args.amount_param
           ~name:"blocks"
           ~desc:"Blocks to mine. < 0 means that will mine forever"
      @@ stop)
      (fun () account amount cctxt ->
        Baking_handler.baking_schedule cctxt account amount ());
  ]
