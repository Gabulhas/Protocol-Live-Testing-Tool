type block_header_data = Alpha_context.Block_header.protocol_data

type block_header = Alpha_context.Block_header.t = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

(*This should be like this*)

let block_header_data_encoding = Alpha_context.Block_header.protocol_data_encoding
(*Encode for the Block header protocol part*)

let max_operation_data_length = 32 * 1024 (*Move this to constants*)
(*Maximum size of an operation *)

let max_block_length = Alpha_context.Block_header.max_header_length
(*Maximum size of the block*)



let validation_passes = Updater.[{max_size = 1000; max_op = None}]

let acceptable_pass _op = Some 0

type block_header_metadata = Apply_results.block_metadata
let block_header_metadata_encoding =  Apply_results.block_metadata_encoding

(** The economic protocol-specific type of operations. *)
(*OPeration data is the same data as the operation itself*)
type operation_data = Alpha_context.Operation.protocol_data
(*The data expected by the operation
For example, in a multitude of different type of operations, you have a peer to peer transaction, that peer to peer transaction should include the source, the destination and the amount of the money

 *)

(** Result o applying an operation *)
type operation_receipt = 
(*
The result of the operation 
This will be returned to whoever injected the operation (basically, whoever sent the operation to the network)
 *)


type operation = Alpha_context.operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}
(*This should be like this. 
Like every block header, that includes a Protocol Specific Part, and a Shell Part, the operations do have two parts for both the shell and the protocol

 *)


let operation_data_encoding =  (*The encoding of the operation_data.*)

let operation_receipt_encoding =(*The encoding of the operation_receipt.*)

let operation_data_and_receipt_encoding =
  (* Simple merge between operation_data and operation_receipt*)

let acceptable_passes _op = [0]

let relative_position_within_block a b = 0
(*
Function used to compare the order of the operations within a block
If, for example, you may want to include Peer to Peer transaction before Smart Contract transactions, this is the function that does so
 *)

type validation_mode = {}
(*
Not necessarily enforced, but you may want to include a validation mode in validation state including info about the current validation mode you are performing (either being Application, Partial application, Construction, etc) 
 *)

type validation_state = { }
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





let begin_application ~chain_id:_ ~predecessor_context ~predecessor_timestamp ~predecessor_fitness:_ (block_header: block_header) =
    (*Code to be executed upon receiving a block from the network 
    This is the starting process of "apply"/concat the block to the node's copy of the blockchain.
    Check the block header's info and more here before the shell proceeds to apply_operation
     *)


let begin_partial_application ~chain_id:_ ~ancestor_context ~predecessor_timestamp ~predecessor_fitness:_ (block_header: block_header) =

    (*Same as begin_application but mostly used when syncing with other nodes when the last block doesn't match the expected block by the node's own blockchain*)


let begin_construction ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp ~predecessor_level ~predecessor_fitness ~predecessor ~timestamp ?(protocol_data : block_header_data option) () =

    (* This will return the needed information so the baker can construct a block
    This name is somewhat misleading, as the protocol won't build a block by itself, instead, it requires some external trigger (the baker) for it to do so

     *)


let apply_operation ({ctxt; op_count; mode} as data) (operation: operation)  =
    (*
    Code to be executed for every operation included in the block
    It should return the context with the operations applied (remember, the context is immutable)
    For example. if it's a simple money transfer from A to B, it should return a context with the balances of each affected account updated

     *)



let finalize_block {mode; ctxt; op_count} (shell_header: Block_header.shell_header option) : (Updater.validation_result * block_header_metadata, error trace) result Lwt.t=
    (*Code to be executed after a block get's finalize, either finalizing  the construction, partial aplication of full aplication of a block
    It updates the current context and pushes that context (and more info) back up to the shell

     *)




let init chain_id ctxt block_header =

    (*Code to be executed before the protocol gets activated 

    This includes potential:
        - preparing the first block of the protocol 
        - storage "conversion" or transfers
        - Other relevant stuff

    *)
