type block_header_data = Alpha_context.Block_header.protocol_data

type block_header = Alpha_context.Block_header.t = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

(*This should be like this*)

let block_header_data_encoding =
  Alpha_context.Block_header.protocol_data_encoding
(*Encode for the Block header protocol part*)

let max_operation_data_length = 32 * 1024
(*Move this to constants*)
(*Maximum size of an operation *)

let max_block_length = Alpha_context.Block_header.max_header_length
(*Maximum size of the block*)

let validation_passes = Updater.[{max_size = 1000; max_op = None}]

let acceptable_pass _op = Some 0

type block_header_metadata = Apply_results.block_metadata

let block_header_metadata_encoding = Apply_results.block_metadata_encoding

(*OPeration data is the same data as the operation itself*)

(** The economic protocol-specific type of operations. *)
type operation_data = Alpha_context.Operation.protocol_data

(** Result o applying an operation *)
type operation_receipt = Apply_results.operation_result

type operation = Alpha_context.operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

let operation_receipt_encoding = Apply_results.operation_result_encoding

let operation_data_encoding = Operation_repr.protocol_data_encoding

let operation_data_and_receipt_encoding =
  (* we could merge data and receipt encoding for a lighter json *)
  Data_encoding.(
    obj2
      (req "data" operation_data_encoding)
      (req "receipt" operation_receipt_encoding))

let acceptable_passes _op = [0]

let relative_position_within_block (a : operation) (b : operation) : int =
  match (a.protocol_data.content, b.protocol_data.content) with
  | Management a, Management b ->
      if Account_repr.equal a.source b.source then Z.compare a.counter b.counter
      else 0
(*
Function used to compare the order of the operations within a block
If, for example, you may want to include Peer to Peer transaction before Smart Contract transactions, this is the function that does so
 *)

type validation_mode =
  | Partial_application
  | Partial_construction
  | Full_construction of {protocol_data : block_header_data}
  | Application of {protocol_data : block_header_data}

let mode_to_string = function
  | Partial_application -> "Partial_application"
  | Partial_construction -> "Partial_construction"
  | Full_construction {protocol_data} ->
      "Construction " ^ Block_header_repr.protocol_data_to_string protocol_data
  | Application {protocol_data} ->
      "Partial_application "
      ^ Block_header_repr.protocol_data_to_string protocol_data

(*
Not necessarily enforced, but you may want to include a validation mode in validation state including info about the current validation mode you are performing (either being Application, Partial application, Construction, etc) 
 *)

type validation_state = {ctxt : Alpha_context.t; mode : validation_mode}
(*

This info is passed between the functions. 
In case you have information created in begin_application, and then you want to access it in apply_operation (one is executed after the ohter)

 *)

(*
Overview of the functions to be implemented:

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
(* *)
let cache_nonce_from_block_header shell =
  let open Alpha_context.Block_header in
  let shell =
    Block_header.
      {
        level = Int32.of_int 0;
        proto_level = 0;
        predecessor = shell.predecessor;
        timestamp = Time.of_seconds 0L;
        validation_passes = 0;
        operations_hash = shell.operations_hash;
        fitness = [];
        context = Context_hash.zero;
      }
  in
  let protocol_data = Block_header_repr.fake_protocol_data in
  let x = {shell; protocol_data} in
  Block_hash.to_bytes (hash x)

let begin_application ~chain_id ~predecessor_context ~predecessor_timestamp
    ~predecessor_fitness:_ (block_header : block_header) =
  (*Code to be executed upon receiving a block from the network
    This is the starting process of "apply"/concat the block to the node's copy of the blockchain.
    Check the block header's info and more here before the shell proceeds to apply_operation
  *)
  let level = block_header.shell.level in
  let current_timestamp = block_header.shell.timestamp in

  Alpha_context.prepare
    predecessor_context
    ~level
    ~timestamp:predecessor_timestamp
  >>=? fun ctxt ->
  Apply.begin_application
    ctxt
    chain_id
    block_header
    predecessor_timestamp
    current_timestamp
  >|=? fun ctxt ->
  let mode = Application {protocol_data = block_header.protocol_data} in
  {ctxt; mode}

let begin_partial_application ~chain_id ~ancestor_context ~predecessor_timestamp
    ~predecessor_fitness:_ (block_header : block_header) =
  let level = block_header.shell.level in
  let current_timestamp = block_header.shell.timestamp in

  Alpha_context.prepare ancestor_context ~level ~timestamp:predecessor_timestamp
  >>=? fun ctxt ->
  Apply.begin_partial_application
    ctxt
    chain_id
    block_header
    predecessor_timestamp
    current_timestamp
  >|=? fun ctxt ->
  let mode = Partial_application in
  {ctxt; mode}

(*Same as begin_application but mostly used when syncing with other nodes when the last block doesn't match the expected block by the node's own blockchain*)

(*Check if the signature is valid*)

let begin_construction ~chain_id:_ ~predecessor_context:ctxt
    ~predecessor_timestamp ~predecessor_level ~predecessor_fitness:_
    ~predecessor:_ ~timestamp ?(protocol_data : block_header_data option) () =
  let level = Int32.succ predecessor_level in
  Alpha_context.prepare ctxt ~level ~timestamp >>=? fun ctxt ->
  (match protocol_data with
  | None ->
      let mode = Partial_construction in
      Lwt.return (ok (mode, ctxt))
  | Some protocol_data ->
      let mode = Full_construction {protocol_data} in
      Apply.begin_construction
        ctxt
        protocol_data
        level
        predecessor_timestamp
        timestamp
      >|=? fun ctxt -> (mode, ctxt))
  >|=? fun (mode, ctxt) -> {mode; ctxt}

(* This will return the needed information so the baker can construct a block
   This name is somewhat misleading, as the protocol won't build a block by itself, instead, it requires some external trigger (the baker) for it to do so
*)

let apply_operation ({ctxt; _} as data) (operation : operation) =
  (*
    Code to be executed for every operation included in the block
    It should return the context with the operations applied (remember, the context is immutable)
    For example. if it's a simple money transfer from A to B, it should return a context with the balances of each affected account updated

     *)
  Apply.apply_operation ctxt operation >|=? fun (ctxt, result) ->
  ({data with ctxt}, result)

type error += Missing_shell_header

let finalize_block {mode; ctxt} (_ : Block_header.shell_header option) :
    (Updater.validation_result * block_header_metadata, error trace) result
    Lwt.t =
  (*Code to be executed after a block get's finalize, either finalizing  the construction, partial aplication of full aplication of a block
    It updates the current context and pushes that context (and more info) back up to the shell
  *)

  (*If the block included a vote for a new validator, this adds the vote to the context.
    If the vote is more than the majority, then it gets added to the validator set
  *)
  let level = Alpha_context.level ctxt in
  let block_timestamp = Alpha_context.timestamp ctxt in

  (match mode with
  | Partial_construction | Partial_application -> Lwt.return (ok ctxt)
  | Application {protocol_data} | Full_construction {protocol_data} -> (
      match protocol_data.contents.vote with
      | None -> Lwt.return (ok ctxt)
      | Some vote -> Apply.update_validator_set_on_vote ctxt vote))
  >>=? fun ctxt ->
  (*This should include any new validator *)
  (* Tally votes for validators *)
  let fitness = Fitness_repr.fitness_from_level level in

  let commit_message = Format.asprintf "lvl %ld" level in
  let ctxt = Alpha_context.finalize ~commit_message ctxt fitness in
  (ctxt, Apply_results.{level; block_timestamp}) |> ok |> Lwt.return

let init _ ctxt block_header =
  let level = block_header.Block_header.level in
  let timestamp = block_header.timestamp in

  Alpha_context.prepare_first_block ctxt ~level ~timestamp >>=? fun ctxt ->
  let cache_nonce = cache_nonce_from_block_header block_header in
  let fitness = Fitness_repr.{level} in
  let fitness_raw = Fitness_repr.to_raw fitness in
  Alpha_context.Cache.Admin.sync ctxt ~cache_nonce >>= fun ctxt ->
  return (Alpha_context.finalize ctxt fitness_raw)

let rpc_services =
  Services.register () ;
  Services_registration.get_rpc_services ()

let compare_operations _ _ = 0

let value_of_key ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp:_
    ~predecessor_level:pred_level ~predecessor_fitness:_ ~predecessor:_
    ~timestamp =
  let level = Int32.succ pred_level in
  Alpha_context.prepare ctxt ~level ~timestamp >>=? fun ctxt ->
  return (Apply.value_of_key ctxt)

(*Code to be executed before the protocol gets activated

  This includes potential:
      - preparing the first block of the protocol
      - storage "conversion" or transfers
      - Other relevant stuff
*)
