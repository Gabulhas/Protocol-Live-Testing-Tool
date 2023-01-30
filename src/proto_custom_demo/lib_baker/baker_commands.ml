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
      @@ Client_proto_args.account_param ~name:"account" ~desc:"Account b58check"
      @@ stop)
      (fun () account cctxt ->
          Baking_handler.baking_schedule cctxt account ()

      )

  ]
