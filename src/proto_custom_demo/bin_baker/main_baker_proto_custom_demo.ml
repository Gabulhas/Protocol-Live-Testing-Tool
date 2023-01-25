let () =
  Client_commands.register Protocol.hash @@ fun _network ->
  List.map (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
  @@ Baking_commands.baker_commands ()

let select_commands _ _ =
  return
    (List.map
       (Tezos_clic.map_command (new Protocol_client_context.wrap_full))
       (Baking_commands.baker_commands ()))

(* This call is not strictly necessary as the parameters are initialized
   lazily the first time a Sapling operation (validation or forging) is
   done. This is what the client does.
   For a long running binary however it is important to make sure that the
   parameters files are there at the start and avoid failing much later while
   validating an operation. Plus paying this cost upfront means that the first
   validation will not be more expensive. *)
let () = Tezos_sapling.Core.Validator.init_params ()

let () = Client_main_run.run (module Daemon_config) ~select_commands

