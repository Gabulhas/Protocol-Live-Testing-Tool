open Alpha_context
open Operation

type error += (* `Permanent *) InconsistentSources
type error += (* `Permanent *) InvalidTarget
type error += (* `Permanent *) TimestampInTheFuture
type error += (* `Permanent *) InvalidCoinbaseTransaction
type error += (* `Permanent *) MultipleCoinbaseTransactions


let () = 
register_error_kind
    `Permanent
    ~id:"operation.multiple_coibase"
    ~title:"There are multiple coinbase transactions in this context."
    ~description:
      "Multiple Coinbase"
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf "Multiple Coinbase")
    Data_encoding.empty
    (function MultipleCoinbaseTransactions -> Some () | _ -> None)
    (fun () -> MultipleCoinbaseTransactions);
register_error_kind
    `Permanent
    ~id:"operation.invalid_coinbase"
    ~title:"Invalid Coinbase. Reward differs from the one calculated using the formula"
    ~description:
      "Invalid Coinbase."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf "Invalid Coinbase.")
    Data_encoding.empty
    (function InvalidCoinbaseTransaction -> Some () | _ -> None)
    (fun () -> InvalidCoinbaseTransaction);
register_error_kind
    `Permanent
    ~id:"block.invalid_timestamp"
    ~title:"Invalid Timestamp. Way to further in the future"
    ~description:
      "Invalid Timestamp. Way to further in the future"
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "Invalid Timestamp. Way to further in the future")
    Data_encoding.empty
    (function TimestampInTheFuture -> Some () | _ -> None)
    (fun () -> TimestampInTheFuture);
register_error_kind
    `Permanent
    ~id:"operation.inconsistent_sources"
    ~title:"Inconsistent sources in operation pack"
    ~description:
      "The operation pack includes operations from different sources."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The operation pack includes operations from different sources.")
    Data_encoding.empty
    (function InconsistentSources -> Some () | _ -> None)
    (fun () -> InconsistentSources);
register_error_kind
    `Permanent
    ~id:"block.invalid_target"
    ~title:"Invalid/Non matching target"
    ~description:
      "This block has a non matching target."
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "The block has an invalid target.")
    Data_encoding.empty
    (function InvalidTarget -> Some () | _ -> None)
    (fun () -> InvalidTarget)


let calculate_current_reward ctxt level =
  let constants = Alpha_context.constants ctxt in
  let halving_epoch_size = constants.halving_epoch_size in
  let reward_multiplier = constants.reward_multiplier in

  let reward_decrease = Int32.div level halving_epoch_size in
  let reward_decrease = Int32.mul reward_decrease reward_decrease in

  let open Tez_repr in
  ok (div_exn reward_multiplier (Int32.to_int reward_decrease)) |> Lwt.return

(*TODO: Verify all this type conversions*)
let calculate_current_target ctxt level current_timestamp =
  let constants = Alpha_context.constants ctxt in
  let epoch_size = constants.difficulty_adjust_epoch_size in
  let open Int32 in
  let open Compare.Int32 in
  if rem level epoch_size = 0l then Header_storage.get_current_target ctxt
  else
    Header_storage.get_current_target ctxt >>=? fun current_target ->
    Header_storage.last_epoch_time ctxt >>=? fun last_epoch_time ->
    let time_taken =
      Int64.sub
        (Time.to_seconds current_timestamp)
        (Time.to_seconds last_epoch_time)
    in
    let epoch_size_seconds =
      Int64.mul (Time.to_seconds constants.block_time) (Int64.of_int32 epoch_size)
    in
    let time_difference_ratio = Int64.div time_taken epoch_size_seconds in
    Lwt.return (Ok (Target_repr.adjust current_target time_difference_ratio))

let is_coinbase_right coinbase_reward coinbase_transaction_value = 
    let open Tez_repr in
    if equal coinbase_reward coinbase_transaction_value then 
        Lwt.return (ok())
    else
        fail InvalidCoinbaseTransaction

let check_multiple_coinbase coinbase_count =
    let open Compare.Int in
    if coinbase_count > 0 then
        fail MultipleCoinbaseTransactions
    else
        Lwt.return (ok ())

let check_manager_signature ctxt operation management_operation =
  (* Basically:
      if we have a reveal operation, we already know the pk from the operation itself (find source)
      if we dont, we look it up on our storage
      *)
  let find_source (op:management_operation) =
      match op with
        | {source; fee = _ ; counter = _; content = Reveal key} ->
          (source, Some key)
      | {source; fee = _ ; counter = _; content = _ }->
          (source, None)
  in
  let (source, source_key) = find_source management_operation in
  ( match source_key with
  | Some key ->
      return key
  | None ->
      Account.get_manager_key ctxt source )
  >>=? fun public_key ->
  Lwt.return (Operation.check_signature public_key operation)



let apply_manager_operation_content (ctxt: Raw_context.t) operation source =
    match operation with
    | Transaction {amount; destination} ->
            (*TODO: Check if user has enough money*)
        Account.debit_account ctxt source amount >>=? fun ctxt ->
        Account.credit_account ctxt destination amount >|=? fun ctxt ->
            (*This should be changed to "TransactionResult only" in case I need to add multiple transactions supportj*)
            (ctxt, Apply_results.Manager_operation_result{
                operation_result= Applied (Transaction_result{balance_updates = [(Account source, Debited amount, Block_application); (Account source, Credited amount, Block_application)]});
                (*TODO change this*)
                nonce= 0;
            }, 0)

    | Reveal pk ->
        Account.reveal_manager_key ctxt source pk >|=? fun ctxt ->
        (ctxt, Apply_results.Manager_operation_result{
            operation_result= Applied (Reveal_result);
            (*TODO change this*)
            nonce= 0;
        }, 0)
    | Coinbase amount ->
        let level = Raw_context.level ctxt in
        calculate_current_reward ctxt level >>=? fun current_reward ->
        is_coinbase_right current_reward amount >>=? fun () -> 
        Account.credit_account ctxt source amount >|=? fun ctxt ->

        (ctxt, Apply_results.Manager_operation_result{
            operation_result= Applied (Coinbase_result{balance_updates = [(Account source, Credited amount, Block_application)]});
            (*TODO change this*)
            nonce=0 
        }, 1)


open Apply_results

let verify_manager_operation ctxt operation (management_operation: management_operation) = 
    (*
    verificar counter_increment
    verificar signature
    Talvez adicionar mais coisas
    executar mas com backtrace
     *)
    Account.check_counter_increment ctxt management_operation.source management_operation.counter >>=? fun () ->
    check_manager_signature ctxt operation management_operation 


let apply_management_operation ctxt operation management_operation: ((t * operation_result * int, error trace) result Lwt.t) =
    let {source; fee=_; counter=_; content} = management_operation in
    verify_manager_operation ctxt operation management_operation >>=?  fun () ->
    Account.increment_counter ctxt source >>=? fun ctxt -> 
    apply_manager_operation_content ctxt content source
            
let apply_operation_contents ctxt operation operation_contents = 
    match operation_contents with
    | Management op -> apply_management_operation ctxt operation op

let apply_operation ctxt (operation: Operation.operation) previous_coinbase_count:  (t *  operation_result * int, error trace) result Lwt.t= 
    (*TODO:  Make it so it's possible to have more than one operation
    Like: We might want to send a Reveal and a Transaction
     *)
    apply_operation_contents ctxt operation operation.protocol_data.content >>=? fun (ctxt, operation_result, coinbase_count) ->
    check_multiple_coinbase (previous_coinbase_count + coinbase_count) >|=? fun () ->
    (ctxt, operation_result, coinbase_count)




let check_same_target this_target current_target = 
    if not (Z.equal this_target current_target) then
        fail InvalidTarget 
    else
        Lwt.return (ok (()))


        (*
let timestamp_in_future ctxt current_timestamp previous_timestamp = 
    (*
    if it's 2 times in the future, it returns error
     *)
    let constants = Alpha_context.constants ctxt in
    let open Int64 in
    let open Compare.Int64 in
    if Time.diff current_timestamp previous_timestamp > Int64.mul (Time.to_seconds constants.block_time) 2L then
        fail TimestampInTheFuture
    else
        Lwt.return (ok())
        
*)


(*TODO: this should have the same signature as the one beign used in the main.ml*)
let begin_application ctxt (block_header: Alpha_context.Block_header.t) level current_timestamp: (t * Tez_repr.t, error trace) result Lwt.t = 

    let open Proof_of_work in
    

    (*
    TODO: check if epoch is %2016, so the the target input in the function should reflect the new target

    prepare
    Recalculate the current difficulty
    Verify Hash
    Verify prev hash
    verify timestamp (not too far in the future)

     *)

    calculate_current_target ctxt level current_timestamp >>=? fun current_target ->
    check_same_target  block_header.protocol_data.target current_target >>=? fun () ->
    check_block block_header current_target >>=? fun () ->
    calculate_current_reward ctxt level >|=? fun current_reward ->
    (ctxt, current_reward)


let begin_construction ctxt current_timestamp (protocol_data:Alpha_context.Block_header.protocol_data) =
    calculate_current_target ctxt (Raw_context.level ctxt) current_timestamp >>=? fun current_target ->
    check_same_target  protocol_data.target current_target >|=? fun () ->
    ctxt

