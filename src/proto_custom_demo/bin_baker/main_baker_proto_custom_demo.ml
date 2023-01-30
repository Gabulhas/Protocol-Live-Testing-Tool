let () =
  Client_commands.register Protocol.hash @@ fun _network ->
  List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
  @@ Baker_commands.baker_commands ()

let select_commands _ _ =
  return
    (List.map
       (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
       (Baker_commands.baker_commands ()))

let () = Tezos_sapling.Core.Validator.init_params ()

let () = Client_main_run.run (module Daemon_config) ~select_commands
