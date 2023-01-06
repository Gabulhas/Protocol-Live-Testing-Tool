open Alpha_context

let begin_application chain_id predecessor_context predecessor_timestamp predecessor_fitness (block_header: block_header) =
    ()


let verify_operation ctxt operation= 

    match operation.protocol_data.content with
    | Transaction {source; amount; destination;fee=_ ;counter} ->
        Account.check_counter_increment ctxt source counter >>=? fun () ->
        Operation.check_signature source operation >>=? fun () ->



    | Coinbase {} _
            

let apply_operation ctxt payer ~source ~chain_id operation= 



