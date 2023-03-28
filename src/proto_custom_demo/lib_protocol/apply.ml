open Alpha_context
open Operation

type error += (* `Permanent *) InconsistentSources
type error += (* `Permanent *) InvalidTarget of Z.t * Z.t
type error += (* `Permanent *) TimestampInTheFuture


let () = 
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
    ~pp:(fun ppf (tried_target, current_target) ->
      Format.fprintf
        ppf
        "The block has an invalid target. Tried: %s but the current is %s"
        (Target_repr.to_hex_string tried_target)
        (Target_repr.to_hex_string current_target)
    )
    Data_encoding.(obj2 (req "tried" Target_repr.encoding) (req "current" Target_repr.encoding))
    (function InvalidTarget(tried_target, current_target) -> Some (tried_target, current_target) | _ -> None)
    (fun (tried_target, current_target) -> InvalidTarget(tried_target, current_target))


let calculate_current_reward ctxt level =
  let constants = Alpha_context.constants ctxt in
  let halving_epoch_size = constants.halving_epoch_size in
  let reward_multiplier = constants.reward_multiplier in

  let reward_decrease_intpart = (Int32.to_int level) / (Int32.to_int halving_epoch_size) in

  let reward_decrease = Utils.integer_positive_pow 2 reward_decrease_intpart in
  
  let current_reward = Tez_repr.div_exn reward_multiplier reward_decrease in

  ok (current_reward) |> Lwt.return


let calculate_current_target ctxt level current_timestamp =
  let constants = Alpha_context.constants ctxt in
  let epoch_size = constants.difficulty_adjust_epoch_size in
  let open Int32 in
  let open Compare.Int32 in
  Logging.log Notice "calculate_current_target: Level: %s | Epoch_size: %s | Reminder %s" 
      (level |> Int32.to_string)
      (epoch_size |> Int32.to_string)
      (rem level epoch_size |> Int32.to_string)
      ;
  if rem level epoch_size <> 0l then Header_storage.get_current_target ctxt >>=? fun current_target -> 
      (current_target, ctxt) |> ok |> Lwt.return
  else
    Header_storage.get_current_target ctxt >>=? fun current_target ->
    Header_storage.last_epoch_time ctxt >>=? fun last_epoch_time ->
    let time_taken =
        Int64.sub
    (Time.to_seconds current_timestamp)
    (Time.to_seconds last_epoch_time)
  in
    Logging.log Notice "calculate_current_target: current_target %s | current_timestamp %s | last_epoch_time %s | Sub(current_timestamp - last_epoch_time): %s" (current_target |> Target_repr.to_hex_string) (Time.to_notation current_timestamp) (Time.to_notation last_epoch_time ) (Int64.to_string time_taken);
    let epoch_size_seconds =
        Int64.mul (Time.to_seconds constants.block_time) (Int64.of_int32 epoch_size)
            in


    let result_target = Z.div (Z.mul current_target (Z.of_int64 time_taken)) (Z.of_int64 epoch_size_seconds) in
    Logging.log Notice "Temp %s" ( result_target |> Target_repr.to_hex_string) ;

    Logging.log Notice "calculate_current_target: Current_target %s | Last_epoch_time %s | Time_taken: %s | Epoch_size_seconds: %s | New_adjust: %s | Final_adjust: %s" 
        (Target_repr.to_hex_string current_target)
        (Time.to_notation last_epoch_time )
        (time_taken |> Int64.to_string)
        (epoch_size_seconds |> Int64.to_string)
        (result_target |> Target_repr.to_hex_string)
        (result_target |> Target_repr.to_hex_string)
        ;

    Header_storage.update_current_target ctxt result_target >>=? fun (ctxt) ->
    (current_target, ctxt) |> ok |> Lwt.return

    (*Lwt.return (Ok (result_target))
    *)

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
    Account_storage.get_counter ctxt source >>=? fun nonce ->

        match operation with
    | Transaction {amount; destination} ->
            Account.debit_account ctxt source amount >>=? fun ctxt ->
                Account.credit_account ctxt destination amount >|=? fun ctxt ->
                    (*This should be changed to "TransactionResult only" in case I need to add multiple transactions supportj*)
                    (ctxt, Apply_results.Manager_operation_result{
                        operation_result= Applied (Transaction_result{balance_updates = [(Account source, Debited amount, Block_application); (Account source, Credited amount, Block_application)]});
                nonce;
                        })

    | Reveal pk ->
            Account.reveal_manager_key ctxt source pk >|=? fun ctxt ->
                (ctxt, Apply_results.Manager_operation_result{
                    operation_result= Applied (Reveal_result);
            nonce;
                    })


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


let apply_management_operation ctxt operation management_operation: ((t * operation_result, error trace) result Lwt.t) =
    let {source; fee=_; counter=_; content} = management_operation in
    verify_manager_operation ctxt operation management_operation >>=?  fun () ->
        Account.increment_counter ctxt source >>=? fun ctxt -> 
            apply_manager_operation_content ctxt content source

let apply_operation_contents ctxt operation operation_contents = 
    match operation_contents with
    | Management op -> apply_management_operation ctxt operation op

let apply_operation ctxt (operation: Operation.operation):  (t *  operation_result, error trace) result Lwt.t= 
    apply_operation_contents ctxt operation operation.protocol_data.content >|=? fun (ctxt, operation_result) ->
        (ctxt, operation_result)


let credit_miner ctxt miner reward = 
    Account.credit_account ctxt miner reward


let check_same_target this_target current_target = 
    if not (Z.equal this_target current_target) then
        InvalidTarget(this_target, current_target) |> fail
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

let begin_application ctxt (block_header : Alpha_context.Block_header.t) level
    current_timestamp : (t, error trace) result Lwt.t =
  let open Proof_of_work in
  calculate_current_target ctxt level current_timestamp
  >>=? fun (current_target, ctxt) ->
  calculate_current_reward ctxt (Raw_context.level ctxt)
  >>=? fun current_reward ->
  credit_miner ctxt block_header.protocol_data.miner current_reward
  >>=? fun ctxt ->
  check_same_target block_header.protocol_data.target current_target
  >>=? fun () ->
  check_block block_header current_target >|=? fun () -> ctxt

let begin_construction ctxt current_timestamp
    (protocol_data : Alpha_context.Block_header.protocol_data) =
  calculate_current_target ctxt (Raw_context.level ctxt) current_timestamp
  >>=? fun (current_target, ctxt) ->
  calculate_current_reward ctxt (level ctxt) >>=? fun current_reward ->
  credit_miner ctxt protocol_data.miner current_reward >>=? fun ctxt ->
  check_same_target protocol_data.target current_target >|=? fun () -> ctxt

let value_of_key ctxt k = Alpha_context.Cache.Admin.value_of_key ctxt k

