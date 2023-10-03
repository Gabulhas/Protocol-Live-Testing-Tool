open Protocol
open Alpha_context
open Block_header_repr
open Protocol_client_context

type error += UnexpectedValidator

let baking_worker (cctxt : Protocol_client_context.full) state validator =
  Client_keys.get_key cctxt validator >>=? fun (_, _, src_sk) ->
  Client_proto_commands.fetch_all_constants cctxt () >>=? fun constants ->
  let parametric = constants.parametric in

  let tolerance = parametric.tolerance in
  let block_time = parametric.block_time in

  Shell_services.Chain.chain_id cctxt ~chain:cctxt#chain () >>=? fun chain_id ->
  let sleep_time =
    Int64.to_float @@ Int64.div (Time.Protocol.to_seconds block_time) 4L
  in

  let get_time_to_sleep previous_timestamp =
    let current_timestamp_seconds = Time.System.now () in
    let current_timestamp = Time.System.to_protocol current_timestamp_seconds in
    let possible_skips =
      Round_selection.calculate_skips
        (Time.Protocol.to_seconds current_timestamp)
        (Time.Protocol.to_seconds previous_timestamp)
        (Time.Protocol.to_seconds block_time)
        (Time.Protocol.to_seconds tolerance)
    in
    let wait_time =
      Int64.mul
        (Time.Protocol.to_seconds block_time)
        (Int64.add 1L possible_skips)
    in
    let next_block_time =
      Int64.add (Time.Protocol.to_seconds previous_timestamp) wait_time
    in

    let time_to_wait =
      Int64.sub next_block_time (Time.Protocol.to_seconds current_timestamp)
    in

    Lwt.return_ok
      (if time_to_wait > 0L then Int64.to_float time_to_wait else 0.)
  in

  let create_and_inject_signed_block (cctxt : Protocol_client_context.full)
      previous_timestamp =
    get_time_to_sleep previous_timestamp >>=? fun sleep_time ->
    Lwt_io.printf
      "Proposer %s is proposing in for %0.2f seconds"
      (Account_repr.to_b58check validator)
      sleep_time
    >>= fun () ->
    Lwt_unix.sleep sleep_time >>= fun () ->
    (* Get new possible block and operations *)
    Header_creation.get_new_possible_block cctxt state validator
    >>=? fun (new_block, operations) ->
    Lwt_io.printf "Successfully preapplied a block" >>= fun () ->
    let preapplied_contents = new_block.protocol_data.contents in
    (* Prepare the contents *)
    let contents =
      {
        validator;
        authority_list = preapplied_contents.authority_list;
        vote = preapplied_contents.vote;
      }
    in

    (* Serialize unsigned header for signing *)
    let unsigned_header =
      Data_encoding.Binary.to_bytes_exn
        Alpha_context.Block_header.unsigned_encoding
        (new_block.shell, contents)
    in

    (* Sign the block *)
    Client_keys.sign
      cctxt
      src_sk
      ~watermark:Block_header.(to_watermark (Block_header chain_id))
      unsigned_header
    >>=? fun signature ->
    (* Create fully signed protocol_data *)
    let protocol_data = {contents; signature} in

    (* Serialize the protocol data *)
    let protocol_bytes =
      Data_encoding.Binary.to_bytes_exn protocol_data_encoding protocol_data
    in
    (* Create and encode the header *)
    let open Tezos_base in
    let header : Block_header.t =
      {shell = new_block.shell; protocol_data = protocol_bytes}
    in
    let header_encoded =
      Data_encoding.Binary.to_bytes_exn Block_header.encoding header
    in

    (* Inject the block *)
    Shell_services.Injection.block cctxt header_encoded operations
    >>= fun block_hash ->
    (match block_hash with
    | Error err ->
        let formatted_err =
          Format.asprintf "%a" (TzTrace.pp_print Error_monad.pp) err
        in
        cctxt#message "Failed to inject new block: %s" formatted_err
    | Ok block_hash ->
        cctxt#message
          "Injected new block %s"
          (block_hash |> Block_hash.to_hex |> function `Hex e -> e))
    >>= fun () -> return ()
  in

  let check_if_validator_is_current_proposer level previous_timestamp =
    let current_timestamp = Time.System.now () |> Time.System.to_protocol in
    Client_proto_commands.validator_set cctxt >>=? fun validators ->
    match
      Round_selection.get_validator
        validators
        level
        (Time.Protocol.to_seconds current_timestamp)
        (Time.Protocol.to_seconds previous_timestamp)
        (Time.Protocol.to_seconds block_time)
        (Time.Protocol.to_seconds tolerance)
    with
    | None -> Lwt.fail (Failure "Failed to fetch expected validator")
    | Some expected ->
        cctxt#message
          "Expected validator: %s. Im %s"
          (Account_repr.to_b58check expected)
          (Account_repr.to_b58check validator)
        >>= fun () ->
        let is_current_proposer = Account_repr.equal expected validator in
        Lwt.return_ok is_current_proposer
  in

  let rec loop () =
    Alpha_block_services.Header.shell_header cctxt () >>=? fun current_shell ->
    check_if_validator_is_current_proposer
      (Int32.succ current_shell.level)
      current_shell.timestamp
    >>=? fun is_current_proposer ->
    (if is_current_proposer then
     create_and_inject_signed_block cctxt current_shell.timestamp
    else Lwt.return_ok ())
    >>=? fun _ ->
    Lwt_unix.sleep sleep_time >>= fun () ->
    Lwt.return_ok () >>=? fun _ -> loop ()
  in
  loop ()

let start_baking_worker (cctxt : Protocol_client_context.full) state validator =
  cctxt#message
    "Validator %s: Starting baking worker"
    (Account_repr.to_b58check validator)
  >>= fun () -> baking_worker cctxt state validator
