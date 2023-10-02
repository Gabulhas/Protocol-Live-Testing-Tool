open Protocol_client_context
open Protocol
open Block_header_repr

let get_new_header_protocol_data_contents cctxt account =
  Client_proto_commands.validator_set cctxt >>=? fun validator_set ->
  cctxt#message
    "Building new protocol data contents with validator set %s "
    (String.concat ", " (List.map Account_repr.to_b58check validator_set))
  >>= fun () ->
  return {validator = account; authority_list = validator_set; vote = None}

(*Pre because it doens't have the nonce yet*)
let get_new_possible_block cctxt state account =
  let time = Time.System.now () |> Time.System.to_protocol in
  get_new_header_protocol_data_contents cctxt account >>=? fun contents ->
  Operation_handler.get_latest_operations state >>= fun latest_operations ->
  let protocol_data = {signature = Signature.zero; contents} in
  cctxt#message "Got %d operations" (List.length latest_operations)
  >>= fun () ->
  Alpha_block_services.Helpers.Preapply.block
    cctxt
    ~timestamp:time
    ~protocol_data
    latest_operations
  >>=? fun (shell_header, preapply_result) ->
  let operations =
    List.map (fun l -> List.map snd l.Preapply_result.applied) preapply_result
  in
  Lwt.return_ok ({shell = shell_header; protocol_data}, operations)
