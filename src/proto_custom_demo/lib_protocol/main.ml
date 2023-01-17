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
type operation_data = Alpha_context.Operation.operation

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
      current_reward : Tez_repr.t;
    }
  | Partial_application of {
      block_header : Alpha_context.Block_header.t;
      current_reward : Tez_repr.t;
    }
  | Partial_construction of {
      predecessor : Block_hash.t
  }
  | Full_construction of {
      predecessor : Block_hash.t;
      protocol_data : Alpha_context.Block_header.contents;
    }

type validation_state = {
  ctxt : Alpha_context.t;
  op_count : int;
  mode: validation_mode;
  coinbase_count: int;
}



(*
THIS IS USED TO INIT THE CHAIN, LIKE THE GENESIS BLOCK AND STUFF

 *)
let init ctxt block_header =
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in
  Alpha_context.prepare_first_block ctxt ~level ~timestamp 
  >|=? fun ctxt -> Alpha_context.finalize ctxt


(*
Enforced signatures:

begin_application: Context.t -> block_header -> validation_state
    is used when validating a block received from the network.

begin_partial_application: Context.t -> block_header -> validation_state
    is used when the shell receives a block more than one level ahead of the current head (this happens, for instance, when synchronizing a node). This function should run quickly, as its main role is to reject invalid blocks from the chain as early as possible. 

begin_construction: Context.t -> ?protocol_data: block_header_data -> validation_state
     is used by the shell when instructed to build a block and for validating operations as they are gossiped on the network. This two cases are distinguished by the optional protocol_data argument: when only validating operations the argument is missing, as there is no block header. In both of these cases, the operations are not (yet) part of a block which is why the function does not expect a shell block header.
    
apply_operation: validation_state -> operation -> validation_state
     is called after begin_application or begin_construction, and before finalize_block, for each operation in the block or in the mempool, respectively. Its role is to validate the operation and to update the (intermediary) state accordingly.
    
finalize_block: validation_state -> validation_result
    represents the last step in a block validation sequence. It produces the context that will be used as input for the validation of the block’s successor candidates.
    

 *)


let begin_application (predecessor_context: Context.t) predecessor_timestamp (block_header: block_header) =
    let level = block_header.shell.level in
    let ctxt = predecessor_context in
    let timestamp =block_header.shell.timestamp  in 
    Alpha_context.prepare ctxt ~level ~timestamp:predecessor_timestamp >>=? fun ctxt ->
    Apply.begin_application ctxt block_header level timestamp >|=? fun (ctxt, current_reward) ->
    let mode =
        Application {block_header; current_reward} in
    {ctxt; op_count=0; mode; coinbase_count=0}
    (*OP count should be 0 because we are just verifying the header*)


let begin_partial_application (predecessor_context: Context.t) predecessor_timestamp (block_header: block_header) =
    let level = block_header.shell.level in
    let ctxt = predecessor_context in
    let timestamp =block_header.shell.timestamp  in 
    Alpha_context.prepare ctxt ~level ~timestamp:predecessor_timestamp >>=? fun ctxt ->
    Apply.begin_application ctxt block_header level timestamp >|=? fun (ctxt, current_reward) ->
    let mode =
        Partial_application {block_header; current_reward} in
    {ctxt; op_count=0; mode; coinbase_count=0}

let begin_construction ~predecessor_context:ctxt
    ~predecessor_timestamp ~predecessor_level:pred_level
    ~predecessor 
    ?(protocol_data : block_header_data option) () =
  let level = Int32.succ pred_level in
  Alpha_context.prepare ctxt ~level ~timestamp:predecessor_timestamp >>=? fun ctxt ->
  
  ( match protocol_data with
  | None ->

      let mode = Partial_construction {predecessor} in
      Lwt.return (ok(mode, ctxt))

  | Some protocol_data ->
      let mode =
        Full_construction {predecessor; protocol_data}
      in
      Apply.begin_construction ctxt predecessor_timestamp protocol_data >|=? fun ctxt ->
      (mode, ctxt)
  )
  >|=? fun (mode, ctxt) ->
  {mode; ctxt; op_count = 0; coinbase_count=0}



(*TODO: check if there are more than 1 coinbase transactions and if the reward differs*)
let apply_operation ({ctxt; op_count; mode; coinbase_count} as data) (operation: operation_data)  =
  let open Apply_results in
  match mode with
  | Partial_application _ ->
      let op_count = op_count + 1 in
      return ({data with ctxt; op_count}, No_result)
  | _ ->
      Apply.apply_operation
        ctxt
        operation
        coinbase_count
      >|=? fun (ctxt, result, coinbase_count) ->
      let op_count = op_count + 1 in

      ({data with ctxt; op_count; coinbase_count}, result)

let finalize_block {mode; ctxt; op_count; coinbase_count} : (Updater.validation_result * block_header_metadata, error trace) result Lwt.t=
  match mode with
  | Partial_construction _ ->
      let level = Alpha_context.level ctxt in
      let block_timestamp = Alpha_context.timestamp ctxt in
      let ctxt = Alpha_context.finalize ctxt in
      ( ctxt, Apply_results.{ 
          level;
          block_timestamp;

        }) |> ok |> Lwt.return
  | Partial_application _ ->
      let level = Alpha_context.level ctxt in
      let block_timestamp = Alpha_context.timestamp ctxt in
      let ctxt = Alpha_context.finalize ctxt in
      ( ctxt, Apply_results.{ 
          level;
          block_timestamp;

        }) |> ok |> Lwt.return



  | Application _
  | Full_construction _ ->
      let level = Alpha_context.level ctxt in
      let block_timestamp = Alpha_context.timestamp ctxt in
      let commit_message =
        Format.asprintf
          "lvl %ld, %d ops, %d coinbases"
          level
          op_count
          coinbase_count
      in
      let ctxt = Alpha_context.finalize ~commit_message ctxt in
      ( ctxt, Apply_results.{ 
          level;
          block_timestamp;

        }) |> ok |> Lwt.return



let rpc_services = ()


let init ctxt block_header =
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in
  Alpha_context.prepare_first_block ctxt ~level ~timestamp 
  >|=? fun ctxt -> Alpha_context.finalize ctxt





(*TODO: Search for "EPOCH"*)

