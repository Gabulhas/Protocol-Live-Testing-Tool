open Alpha_context
open Operation

type error += (* `Permanent *) ProposerNotAValidator of Account_repr.t

type error +=
  | (* `Permanent *)
      ProposeInWrongRound of
      Account_repr.t * Int32.t * Int64.t * Account_repr.t

type error += (* `Permanent *) InconsistentSignature

type error += (* `Permanent *) BlockSkippedOrTooSoon of Int32.t * Time.t

type error +=
  | (* `Permanent *)
      ValidatorAuthorityListMismatch of
      Account_repr.t list * Account_repr.t list

let () =
  (* ProposerNotAValidator *)
  register_error_kind
    `Permanent
    ~id:"block.proposer_not_a_validator"
    ~title:"Proposer is not a Validator"
    ~description:"The proposer is not a registered validator."
    ~pp:(fun ppf acc ->
      Format.fprintf
        ppf
        "The proposer %s is not a registered validator."
        (Account_repr.to_b58check acc))
    Data_encoding.(obj1 (req "account" Account_repr.encoding))
    (function ProposerNotAValidator acc -> Some acc | _ -> None)
    (fun acc -> ProposerNotAValidator acc) ;

  (* ProposeInWrongRound *)
  register_error_kind
    `Permanent
    ~id:"block.propose_in_wrong_round"
    ~title:"Propose in Wrong Round"
    ~description:"The proposer attempted to propose in the wrong round."
    ~pp:(fun ppf (acc, round, skips, expected) ->
      Format.fprintf
        ppf
        "The proposer %s attempted to propose in round %s and skipped %s \
         times. %s was expected."
        (Account_repr.to_b58check acc)
        (Int32.to_string round)
        (Int64.to_string skips)
        (Account_repr.to_b58check expected))
    Data_encoding.(
      obj4
        (req "account" Account_repr.encoding)
        (req "round" int32)
        (req "skips" int64)
        (req "expected" Account_repr.encoding))
    (function
      | ProposeInWrongRound (acc, round, skips, expected) ->
          Some (acc, round, skips, expected)
      | _ -> None)
    (fun (acc, round, skips, expected) ->
      ProposeInWrongRound (acc, round, skips, expected)) ;

  (* InconsistentSignature *)
  register_error_kind
    `Permanent
    ~id:"block.inconsistent_signature"
    ~title:"Inconsistent Signature"
    ~description:"The block has an inconsistent signature."
    ~pp:(fun ppf () ->
      Format.pp_print_string ppf "The block has an inconsistent signature.")
    Data_encoding.empty
    (function InconsistentSignature -> Some () | _ -> None)
    (fun () -> InconsistentSignature) ;

  (* BlockSkippedOrTooSoon *)
  register_error_kind
    `Permanent
    ~id:"block.skipped_or_too_soon"
    ~title:"Block Skipped or Too Soon"
    ~description:"The block was either skipped or proposed too soon."
    ~pp:(fun ppf (round, time) ->
      Format.fprintf
        ppf
        "The block for round %s was skipped or proposed too soon at time %s."
        (Int32.to_string round)
        (Time.to_notation time))
    Data_encoding.(obj2 (req "round" int32) (req "time" Time.encoding))
    (function
      | BlockSkippedOrTooSoon (round, time) -> Some (round, time) | _ -> None)
    (fun (round, time) -> BlockSkippedOrTooSoon (round, time)) ;

  (* ValidatorAuthorityListMismatch *)
  register_error_kind
    `Permanent
    ~id:"block.validator_authority_list_mismatch"
    ~title:"Validator and Authority List Mismatch"
    ~description:
      "The validator set and the authority list in the header do not match."
    ~pp:(fun ppf (validators, authorities) ->
      Format.fprintf
        ppf
        "Validator set and Authority list do not match. Validators: %s, \
         Authorities: %s"
        (String.concat ", " (List.map Account_repr.to_b58check validators))
        (String.concat ", " (List.map Account_repr.to_b58check authorities)))
    Data_encoding.(
      obj2
        (req "validators" (list Account_repr.encoding))
        (req "authorities" (list Account_repr.encoding)))
    (function
      | ValidatorAuthorityListMismatch (validators, authorities) ->
          Some (validators, authorities)
      | _ -> None)
    (fun (validators, authorities) ->
      ValidatorAuthorityListMismatch (validators, authorities))

let check_manager_signature ctxt operation management_operation =
  (* Basically:
       if we have a reveal operation, we already know the pk from the operation itself (find source)
     if we dont, we look it up on our storage
  *)
  let find_source (op : management_operation) =
    match op with
    | {source; fee = _; counter = _; content = Reveal key} -> (source, Some key)
    | {source; fee = _; counter = _; content = _} -> (source, None)
  in
  let source, source_key = find_source management_operation in
  (match source_key with
  | Some key -> return key
  | None -> Account.get_manager_key ctxt source)
  >>=? fun public_key ->
  Lwt.return (Operation.check_signature public_key operation)

let apply_manager_operation_content (ctxt : Raw_context.t) operation source =
  Account_storage.get_counter ctxt source >>=? fun nonce ->
  match operation with
  | Transaction {amount; destination} ->
      Account.debit_account ctxt source amount >>=? fun ctxt ->
      Account.credit_account ctxt destination amount >|=? fun ctxt ->
      (*This should be changed to "TransactionResult only" in case I need to add multiple transactions supportj*)
      ( ctxt,
        Apply_results.Manager_operation_result
          {
            operation_result =
              Applied
                (Transaction_result
                   {
                     balance_updates =
                       [
                         (Account source, Debited amount, Block_application);
                         (Account source, Credited amount, Block_application);
                       ];
                   });
            nonce;
          } )
  | Reveal pk ->
      Account.reveal_manager_key ctxt source pk >|=? fun ctxt ->
      ( ctxt,
        Apply_results.Manager_operation_result
          {operation_result = Applied Reveal_result; nonce} )

open Apply_results

let verify_manager_operation ctxt operation
    (management_operation : management_operation) =
  (*
    verificar counter_increment
    verificar signature
    Talvez adicionar mais coisas
    executar mas com backtrace
     *)
  Account.check_counter_increment
    ctxt
    management_operation.source
    management_operation.counter
  >>=? fun () -> check_manager_signature ctxt operation management_operation

let apply_management_operation ctxt operation management_operation :
    (t * operation_result, error trace) result Lwt.t =
  let {source; fee = _; counter = _; content} = management_operation in
  verify_manager_operation ctxt operation management_operation >>=? fun () ->
  Account.increment_counter ctxt source >>=? fun ctxt ->
  apply_manager_operation_content ctxt content source

let apply_operation_contents ctxt operation operation_contents =
  match operation_contents with
  | Management op -> apply_management_operation ctxt operation op

let apply_operation ctxt (operation : Operation.operation) :
    (t * operation_result, error trace) result Lwt.t =
  apply_operation_contents ctxt operation operation.protocol_data.content
  >|=? fun (ctxt, operation_result) -> (ctxt, operation_result)

let check_block_signature ctxt chain_id
    (block_header : Alpha_context.Block_header.t) =
  let protocol_data = block_header.protocol_data in
  Account_storage.get_manager_key ctxt protocol_data.contents.validator
  >>=? fun validator_pk ->
  match Block_header.check_signature block_header chain_id validator_pk with
  | Error e -> Lwt.return (Error e)
  | Ok _ -> Lwt.return (ok ())

let is_proposer_a_validator_in_storage ctxt proposer =
  Validator_set_storage.is_validator ctxt proposer >>=? fun res ->
  if res then Lwt.return (ok ()) else ProposerNotAValidator proposer |> fail

let authority_list_equals_validator_set ctxt
    (block_header : Alpha_context.Block_header.t) =
  let authority_list = block_header.protocol_data.contents.authority_list in
  Validator_set_storage.get_validator_set ctxt >>=? fun validator_set ->
  if List.equal Account_repr.equal validator_set authority_list then
    Lwt.return (ok ())
  else ValidatorAuthorityListMismatch (validator_set, authority_list) |> fail

let is_within_tolerance ctxt level (predecessor_timestamp : Time.t)
    (current_timestamp : Time.t) =
  Logging.log
    Logging.Notice
    "Checking time tolerance. Level: %s, Predecessor Time: %s, Current Time: %s"
    (Int32.to_string level)
    (Time.to_seconds predecessor_timestamp |> Int64.to_string)
    (Time.to_seconds current_timestamp |> Int64.to_string) ;

  let constants = Alpha_context.constants ctxt in
  let tolerance = constants.tolerance in
  let block_time = constants.block_time in
  let lower_bound, expected, upper_bound =
    Round_selection.get_block_time_bounds
      current_timestamp
      predecessor_timestamp
      block_time
      tolerance
  in

  Logging.log
    Logging.Notice
    "Expected Timestamp: %s, Lower Bound: %s, Upper Bound: %s"
    (expected |> Int64.to_string)
    (lower_bound |> Int64.to_string)
    (upper_bound |> Int64.to_string) ;

  if
    Time.(
      current_timestamp < Time.of_seconds lower_bound
      || current_timestamp > Time.of_seconds upper_bound)
  then (
    Logging.log Logging.Notice "Block time out of tolerance. Failing." ;
    BlockSkippedOrTooSoon (level, current_timestamp) |> fail)
  else (
    Logging.log Logging.Notice "Block time within tolerance. Proceeding." ;
    Lwt.return (ok ()))

let is_validator_the_current_proposer ctxt proposer validators round
    (current_timestamp : Time.t) (predecessor_timestamp : Time.t) =
  let constants = Alpha_context.constants ctxt in
  let tolerance = constants.tolerance |> Time.to_seconds in
  let block_time = constants.block_time |> Time.to_seconds in
  let current_timestamp = current_timestamp |> Time.to_seconds in
  let predecessor_timestamp = predecessor_timestamp |> Time.to_seconds in
  let expected_validator_opt =
    Round_selection.get_validator
      validators
      round
      current_timestamp
      predecessor_timestamp
      block_time
      tolerance
  in
  let expected_validator =
    match expected_validator_opt with None -> Account_repr.zero | Some a -> a
  in

  if Account_repr.equal expected_validator proposer then Lwt.return (ok ())
  else
    let skips =
      Round_selection.calculate_skips
        current_timestamp
        predecessor_timestamp
        block_time
        tolerance
    in
    fail
      (ProposeInWrongRound
         ( proposer,
           round,
           skips,
           match
             Round_selection.expected_validator_address_with_skips
               validators
               round
               skips
           with
           | Some a -> a
           | None -> Account_repr.zero ))

let begin_application ctxt chain_id
    (block_header : Alpha_context.Block_header.t) predecessor_timestamp
    current_timestamp : (t, error trace) result Lwt.t =
  (*Check if the node is a validator*)
  let proposer = block_header.protocol_data.contents.validator in
  let level = block_header.shell.level in
  let current_round = level in
  Validator_set_storage.get_validator_set ctxt >>=? fun validator_set ->
  is_proposer_a_validator_in_storage ctxt proposer >>=? fun _ ->
  is_validator_the_current_proposer
    ctxt
    proposer
    validator_set
    current_round
    current_timestamp
    predecessor_timestamp
  >>=? fun _ ->
  check_block_signature ctxt chain_id block_header >>=? fun _ ->
  (*is_within_tolerance ctxt level predecessor_timestamp current_timestamp >>=? fun _ ->*)
  authority_list_equals_validator_set ctxt block_header >|=? fun _ -> ctxt

(*Check if the node is actually the one baking*)
(*Check if the signature is valid*)
(*Check if the current_timestamp is valid and within a treshodl*)

let begin_partial_application ctxt chain_id
    (block_header : Alpha_context.Block_header.t) predecessor_timestamp
    current_timestamp : (t, error trace) result Lwt.t =
  (*Check if the node is a validator in the block_header*)
  (*Check if the signature is valid*)
  let proposer = block_header.protocol_data.contents.validator in
  let level = block_header.shell.level in
  let current_round = level in
  let authority_list = block_header.protocol_data.contents.authority_list in
  is_validator_the_current_proposer
    ctxt
    proposer
    authority_list
    current_round
    current_timestamp
    predecessor_timestamp
  (*>>=? fun _ ->
    is_within_tolerance ctxt level predecessor_timestamp current_timestamp*)
  >>=?
  fun _ ->
  check_block_signature ctxt chain_id block_header >|=? fun _ -> ctxt

let begin_construction ctxt
    (protocol_data : Alpha_context.Block_header.protocol_data) level
    predecessor_timestamp current_timestamp : (t, error trace) result Lwt.t =
  let proposer = protocol_data.contents.validator in
  let current_round = level in
  Validator_set_storage.get_validator_set ctxt >>=? fun validator_set ->
  is_proposer_a_validator_in_storage ctxt proposer >>=? fun _ ->
  is_validator_the_current_proposer
    ctxt
    proposer
    validator_set
    current_round
    current_timestamp
    predecessor_timestamp
  (*>>=? fun _ ->
    is_within_tolerance ctxt level predecessor_timestamp current_timestamp*)
  >|=?
  fun _ -> ctxt

(* *)
let value_of_key ctxt k = Alpha_context.Cache.Admin.value_of_key ctxt k

let vote_result (vote_set : Vote_repr.stored_action list) =
  let open Vote_repr in
  List.fold_left
    (fun (add, remove) vote ->
      match vote.action with
      | Add -> (add + 1, remove)
      | Remove -> (add, remove)
      | Ignore -> (add, remove))
    (0, 0)
    vote_set

let update_validator_set_on_vote ctxt (new_vote : Vote_repr.vote) =
  Account.add_new_vote_to_account ctxt new_vote >>=? fun ctxt ->
  Account.get_vote_set ctxt new_vote.candidate >>=? fun vote_set ->
  Validator_set_storage.get_validator_set ctxt >>=? fun validator_set ->
  let add, remove = vote_result vote_set in

  let validator_set_size = List.length validator_set in
  let majority = (validator_set_size / 2) + 1 in
  let open Compare.Int in
  if add >= majority then
    Validator_set_storage.add_to_validator_set ctxt new_vote.candidate
  else if remove >= majority then
    Validator_set_storage.remove_from_validator_set ctxt new_vote.candidate
  else Lwt.return (ok ctxt)
