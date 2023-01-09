open Alpha_context
open Operation

type error += (* `Permanent *) Inconsistent_sources


let () = 
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
    (function Inconsistent_sources -> Some () | _ -> None)
    (fun () -> Inconsistent_sources)


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


open Apply_results
open Receipt

let apply_manager_operation_content ctxt operation source =
    match operation with
    | Transaction {amount; destination} ->
            (*TODO: Check if user has enough money*)
        Account.debit_account ctxt source amount >>=? fun ctxt ->
        Account.credit_account ctxt destination amount >|=? fun ctxt ->
            (*This should be changed to "TransactionResult only" in case I need to add multiple transactions supportj*)
            (ctxt, Manager_operation_result{
                operation_result= Applied (Transaction_result{balance_updates = [(Account source, Debited amount, Block_application); (Account source, Credited amount, Block_application)]});
                (*TODO change this*)
                nonce= 0;
            })

    | Reveal pk ->
        Account.reveal_manager_key ctxt source pk >|=? fun ctxt ->
        (ctxt, Manager_operation_result{
            operation_result= Applied (Reveal_result);
            (*TODO change this*)
            nonce= 0;
        })
    | Coinbase amount ->
        Account.credit_account ctxt source amount >|=? fun ctxt ->
        (ctxt, Manager_operation_result{
            operation_result= Applied (Reveal_result);
            (*TODO change this*)
            nonce=0 
        })


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
    (*TODO:  Make it so it's possible to have more than one operation
    Like: We might want to send a Reveal and a Transaction
     *)
    apply_operation_contents ctxt operation operation.protocol_data.content 

