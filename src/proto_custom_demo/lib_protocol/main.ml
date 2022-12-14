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

type validation_state = {
  chain_id : Chain_id.t;
  ctxt : Alpha_context.t;
  op_count : int;
}

let begin_application chain_id predecessor_context predecessor_timestamp predecessor_fitness (block_header: block_header) =
    let level = block_header.shell.level

    

    (*
  let level = block_header.shell.level in
  let fitness = predecessor_fitness in
  let timestamp = block_header.shell.timestamp in
  Alpha_context.prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt
  >>=? fun (ctxt, migration_balance_updates) ->
  Apply.begin_application ctxt chain_id block_header predecessor_timestamp
  >|=? fun (ctxt, baker, block_delay) ->
  let mode =
    Application
      {block_header; baker = Signature.Public_key.hash baker; block_delay}
  in
  {mode; chain_id; ctxt; op_count = 0; migration_balance_updates}
*)


let begin_partial_application chain_id ancestor_context predecessor_timestamp predecessor_fitness block_header =
    ()

    (** [begin_construction] initializes a validation context for
    constructing a new block, as opposed to validating an existing
    block.

    This function can be used in two modes: with and without the
    optional [protocol_data] argument. With the latter, it is used by
    bakers to start the process for baking a new block. Without it,
    is used by the Shell's prevalidator to construct a virtual block,
    which carries the contents of the pre-applied operations of the
    mempool.

    When [protocol_data] is provided, it is not expected to be the
    final value of the field of the same name in the {!block_header}
    of the block eventually being baked. Instead, it is expected to
    construct a protocol-specific, good enough, "prototype" of its
    final value. For instance, if the economic protocol specifies
    that its block headers include a signature, [protocol_data] must
include a (faked) signature.

Moreover, these prototypes should not be distinguishable after
the application of [begin_construction]: the function must
produce the exact same context regardless of being passed a
prototype, or an "equivalent-but-complete" header. *)
    (*val begin_construction :
        chain_id:Chain_id.t ->
            predecessor_context:Context.t ->
                predecessor_timestamp:Time.t ->
                    predecessor_level:Int32.t ->
                        predecessor_fitness:Fitness.t ->
                            predecessor:Block_hash.t ->
                                timestamp:Time.t ->
                                    ?protocol_data:block_header_data ->
                                        unit ->
                                            validation_state tzresult Lwt.t
                                            *)

let begin_construction chain_id predecessor_context predecessor_timestamp predecessor_level predecessor_fitness predecessor timestamp ?protocol_data unit =
    ()

    (** [apply_operation vs op] applies the input operation [op] on top
    of the given {!validation_state} [vs]. It must be called after
    {!begin_application} or {!begin_construction}, and before
    {!finalize_block}, for each operation in a block. On a successful
    application, it returns a pair consisting of the resulting
    [validation_state], and the corresponding [operation_receipt]. *)
    (*
val apply_operation :
    validation_state ->
        operation ->
            (validation_state * operation_receipt) tzresult Lwt.t
            *)
let apply_operation validation_state operation =
    ()


    (** [finalize_block vs] finalizes the context resulting from the
    application of the contents of the block being validated.

    If there is no protocol migration, i.e., if the block being
    applied is not the last block of the current economic protocol, the
    resulting context can be used in the future as input for the
    validation of its successor blocks. *)
    (*
val finalize_block :
    validation_state ->
        Block_header.shell_header option ->
            (validation_result * block_header_metadata) tzresult Lwt.t
            *)
let finalize_block validation_state block_header = ()


(** [rpc_services] provides the list of remote procedures exported
by this protocol implementation. *)
(*val rpc_services : rpc_context RPC_directory.t*)
let rpc_services = ()

(** [init chain_id ctxt hd] initializes the context, or upgrades the
context after a protocol amendment. This function receives as
arguments the [chain_id] of the current chain and the context
[ctxt] resulting from the application of the block that triggered
the amendment, as well as its header [hd]. This function should
fail if the "protocol stitching", i.e., the transition from a
valid previous protocol to the one being activated, has not been
implemented. *)
(*val init :
    Chain_id.t ->
        Context.t ->
            Block_header.shell_header ->
                validation_result tzresult Lwt.t

                *)
let init chain_id context header = ()

(** [value_of_key chain_id predecessor_context
predecessor_timestamp predecessor_level predecessor_fitness
predecessor timestamp] returns a function to build one value of
the cache from its key.

This function is used to restore all or part of the cache, for
instance when booting a validator to preheat the cache, or when a
reorganization happens. This function should never fail, returned
errors are fatal.

The generated function is passed to [Context.Cache.load_caches]
which will use it either immediately a cache-loading time or
on-demand, when a given cached value is accessed. *)

(*val value_of_key :
    chain_id:Chain_id.t ->
        predecessor_context:Context.t ->
            predecessor_timestamp:Time.t ->
                predecessor_level:Int32.t ->
                    predecessor_fitness:Fitness.t ->
                        predecessor:Block_hash.t ->
                            timestamp:Time.t ->
                                (Context.Cache.key -> Context.Cache.value tzresult Lwt.t) tzresult Lwt.t
                                *)

let value_of_key chain_id predecessor_context predecessor_timestamp predecessor_level predecessor_fitness predecessor_block_hash timestamp = ()
