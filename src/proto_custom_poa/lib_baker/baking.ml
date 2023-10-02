open Protocol
open Alpha_context
open Block_header_repr
open Protocol_client_context

let baking_worker (cctxt : Protocol_client_context.full) state validator =
  Client_keys.get_key cctxt validator >>=? fun (_, _, src_sk) ->
  Client_proto_commands.fetch_all_constants cctxt () >>=? fun constants ->
  let parametric = constants.parametric in
  let tolerance = parametric.tolerance in
  Shell_services.Chain.chain_id cctxt ~chain:cctxt#chain () >>=? fun chain_id ->
  let block_time = parametric.block_time in
  let sleep_time =
    Int64.to_float
    @@ Int64.add
         (Time.Protocol.to_seconds block_time)
         (Time.Protocol.to_seconds tolerance)
  in

  let wait_for_next_block_time previous_timestamp =
    let current_timestamp = Time.System.now () |> Time.System.to_protocol in
    Client_proto_commands.skips_at_timestamp
      cctxt
      previous_timestamp
      current_timestamp
    >>=? fun possible_skips ->
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
      (Lwt_unix.sleep
         (if time_to_wait > 0L then Int64.to_float time_to_wait else 0.))
  in

  let create_and_inject_signed_block (cctxt : Protocol_client_context.full)
      previous_timestamp =
    cctxt#message
      "Time before wait %s"
      (Time.System.now () |> Time.System.to_protocol |> Time.Protocol.to_seconds
     |> Int64.to_string)
    >>= fun () ->
    wait_for_next_block_time previous_timestamp >>= fun _ ->
    cctxt#message
      "Time after wait %s"
      (Time.System.now () |> Time.System.to_protocol |> Time.Protocol.to_seconds
     |> Int64.to_string)
    >>= fun () ->
    (* Get new possible block and operations *)
    Header_creation.get_new_possible_block cctxt state validator
    >>=? fun (new_block, operations) ->
    cctxt#message
      "Fetched new possible block with %d operations"
      (List.length operations)
    >>= fun () ->
    let preapplied_contents = new_block.protocol_data.contents in
    cctxt#message
      "Preapplied contents with validator %s"
      (Account_repr.to_b58check preapplied_contents.validator)
    >>= fun () ->
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
    cctxt#message
      "Validator %s: Serialized unsigned header"
      (Account_repr.to_b58check validator)
    >>= fun () ->
    (* Sign the block *)
    Client_keys.sign
      cctxt
      src_sk
      ~watermark:Block_header.(to_watermark (Block_header chain_id))
      unsigned_header
    >>=? fun signature ->
    cctxt#message
      "Signed block with signature %s"
      (Signature.to_string signature)
    >>= fun () ->
    (* Create fully signed protocol_data *)
    let protocol_data = {contents; signature} in

    (* Serialize the protocol data *)
    let protocol_bytes =
      Data_encoding.Binary.to_bytes_exn protocol_data_encoding protocol_data
    in
    cctxt#message
      "Validator %s: Serialized protocol data"
      (Account_repr.to_b58check validator)
    >>= fun () ->
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
    >>=? fun block_hash ->
    cctxt#message
      "Injected new block %s"
      (block_hash |> Block_hash.to_hex |> function `Hex e -> e)
    >>= fun () -> return ()
  in

  let check_if_validator_is_current_proposer level previous_timestamp =
    let current_timestamp = Time.System.now () |> Time.System.to_protocol in
    cctxt#message
      "Current timestamp: %s"
      (Time.Protocol.to_notation current_timestamp)
    >>= fun () ->
    Client_proto_commands.expected_validator
      cctxt
      level
      previous_timestamp
      current_timestamp
    >>=? fun expected_opt ->
    cctxt#message
      "Validator %s: Fetched expected validator for level %ld"
      (Account_repr.to_b58check validator)
      level
    >>= fun () ->
    match expected_opt with
    | None ->
        cctxt#message
          "Validator %s: Failed to fetch expected validator"
          (Account_repr.to_b58check validator)
        >>= fun () -> Lwt.fail (Failure "Failed to fetch expected validator")
    | Some expected ->
        cctxt#message
          "Expected validator: %s"
          (Account_repr.to_b58check expected)
        >>= fun () ->
        let is_current_proposer = Account_repr.equal expected validator in
        cctxt#message
          "Is current validator the proposer: %b"
          is_current_proposer
        >>= fun () -> Lwt.return_ok is_current_proposer
  in

  let rec loop (previous_shell : shell_header) =
    cctxt#message
      "Validator %s: Previous shell level: %ld"
      (Account_repr.to_b58check validator)
      previous_shell.level
    >>= fun () ->
    Alpha_block_services.Header.shell_header cctxt () >>=? fun current_shell ->
    cctxt#message
      "Validator %s: Current shell level: %ld"
      (Account_repr.to_b58check validator)
      current_shell.level
    >>= fun () ->
    (if Compare.Int32.equal current_shell.level previous_shell.level then
     cctxt#message
       "Shell level hasn't changed. Checking if validator is current proposer."
     >>= fun () ->
     check_if_validator_is_current_proposer
       current_shell.level
       previous_shell.timestamp
     >>=? fun is_it ->
     cctxt#message
       "Validator %s: Is validator current proposer: %b"
       (Account_repr.to_b58check validator)
       is_it
     >>= fun () ->
     if is_it then create_and_inject_signed_block cctxt previous_shell.timestamp
     else Lwt.return_ok ()
    else
      cctxt#message
        "Shell level has changed. Fetching first block validator for level."
      >>= fun () ->
      Client_proto_commands.first_block_validator cctxt current_shell.level
      >>=? fun expected ->
      cctxt#message
        "Expected validator for new block: %s"
        (Account_repr.to_b58check expected)
      >>= fun () ->
      if Account_repr.equal expected validator then
        create_and_inject_signed_block cctxt previous_shell.timestamp
      else Lwt.return_ok ())
    >>=? fun _ ->
    cctxt#message
      "Validator %s: Sleeping for %f seconds"
      (Account_repr.to_b58check validator)
      sleep_time
    >>= fun () ->
    Lwt_unix.sleep sleep_time >>= fun () ->
    cctxt#message
      "Validator %s: Waking up to loop again."
      (Account_repr.to_b58check validator)
    >>= fun () ->
    Lwt.return_ok () >>=? fun _ -> loop current_shell
  in

  Alpha_block_services.Header.shell_header cctxt () >>=? fun shell -> loop shell

let start_baking_worker (cctxt : Protocol_client_context.full) state validator =
  cctxt#message
    "Validator %s: Starting baking worker"
    (Account_repr.to_b58check validator)
  >>= fun () -> baking_worker cctxt state validator
