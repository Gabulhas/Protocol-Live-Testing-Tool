type block_header_data = Alpha_context.Block_header.protocol_data

type block_header = Alpha_context.Block_header.t 

let block_header_data_encoding = Alpha_context.Block_header.protocol_data_encoding


let max_block_length = Alpha_context.Block_header.max_header_length

(*TODO: change this to reflect actual values*)
let max_operation_data_length = 32 * 1024 (* 32kB *)

(*Ignored*)
let validation_passes = []


type block_header_metadata = Apply_results.block_metadata
let block_header_metadata_encoding =  Apply_results.block_metadata_encoding

(** The economic protocol-specific type of operations. *)
(*OPeration data is the same data as the operation itself*)
type operation_data = Operation_repr.operation

(** Result o applying an operation *)
type operation_receipt = Apply_results.operation_result

type operation = Alpha_context.operation
let operation_receipt_encoding = Apply_results.operation_result_encoding

let operation_data_encoding = Operation.encoding

let operation_data_and_receipt_encoding =
  (* we could merge data and receipt encoding for a lighter json *)
  Data_encoding.(
    obj2 (req "data" operation_data_encoding) (req "receipt" operation_receipt_encoding))

let acceptable_passes _op = Some 0

let relative_position_within_block _a _b = 0

type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t;
    }
  | Partial_application of {
      block_header : Alpha_context.Block_header.t;
    }
  | Partial_construction of {predecessor : Block_hash.t}
  | Full_construction of {
      predecessor : Block_hash.t;
      protocol_data : Alpha_context.Block_header.contents;
    }

type validation_state = {
  chain_id : Chain_id.t;
  ctxt : Alpha_context.t;
  op_count : int;
  mode: validation_mode;
}



(*
THIS IS USED TO INIT THE CHAIN, LIKE THE GENESIS BLOCK AND STUFF


 *)
let init ctxt block_header =
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in
  Alpha_context.prepare_first_block ctxt ~level ~timestamp 
  >|=? fun ctxt -> Alpha_context.finalize ctxt


let begin_application chain_id (predecessor_context: Context.t) predecessor_timestamp predecessor_fitness (block_header: block_header) =
    let level = block_header.shell.level in
    (*
    TODO: check if epoch is %2016, so the the target input in the function should reflect the new target

     *)
    predecessor_context

    Proof_of_work.check_block block_header  >>= fun is_valid ->
    if !is_valid then
        Proof_of_work.invalid_block_hash ()
    else
        let mode = Application {block_header} in
    {mode; chain_id; ctxt; op_count = 0}
     
    
    


        
        
        


    


let begin_partial_application chain_id ancestor_context predecessor_timestamp predecessor_fitness block_header =
    ()

let begin_construction chain_id predecessor_context predecessor_timestamp predecessor_level predecessor_fitness predecessor timestamp ?protocol_data unit =
    ()

let apply_operation ({mode; chain_id; ctxt; op_count; _} as data) operation  =
    let {shell; protocol_data = Operation_data protocol_data} = operation in
      let operation : _ Alpha_context.operation = {shell; protocol_data} in
      let (predecessor, baker) =
        match mode with
        | Partial_application
            {block_header = {shell = {predecessor; _}; _}; baker}
        | Application {block_header = {shell = {predecessor; _}; _}; baker}
        | Full_construction {predecessor; baker; _} ->
            (predecessor, baker)
        | Partial_construction {predecessor} ->
            (predecessor, Signature.Public_key_hash.zero)
      in
      Apply.apply_operation
        ctxt
        chain_id
        Optimized
        predecessor
        baker
        (Alpha_context.Operation.hash operation)
        operation
      >|=? fun (ctxt, result) ->
      let op_count = op_count + 1 in
      ({data with ctxt; op_count}, Operation_metadata result)


let finalize_block validation_state block_header = ()


let rpc_services = ()


let init chain_id context header = ()
(*TODO: Search for "EPOCH"*)

let value_of_key chain_id predecessor_context predecessor_timestamp predecessor_level predecessor_fitness predecessor_block_hash timestamp = ()
