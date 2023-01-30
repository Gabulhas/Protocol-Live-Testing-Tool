open Protocol_client_context
open Protocol.Block_header_repr

let get_new_header_protocol_data cctxt account =
  let time = Time.System.now () |> Time.System.to_protocol in
  Client_proto_commands.get_current_target cctxt >>=? fun target ->
  (*TODO: change this*)
  return {time; target; nonce = 0L; miner = account}

(*Pre because it doens't have the nonce yet*)
let get_new_possible_block cctxt state account =
  get_new_header_protocol_data cctxt account >>=? fun protocol_data ->
  Operation_handler.get_latest_operations state >>= fun latest_operations ->
  Alpha_block_services.Helpers.Preapply.block
    cctxt
    ~timestamp:protocol_data.time
    ~protocol_data
    latest_operations
  >>=? fun (shell_header, preapply_result) ->
  let operations =
    List.map (fun l -> List.map snd l.Preapply_result.applied) preapply_result
  in
  Lwt.return_ok ({shell = shell_header; protocol_data}, operations)
