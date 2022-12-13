type block_header_data = Alpha_context.Block_header.t

type block_header = Alpha_context.Block_header.t 

let block_header_data_encoding = Alpha_context.Block_header.encoding



let max_block_length = 0 + Alpha_context.Block_header.max_header_length

let max_operation_data_length = 0

(*Ignored*)
let validation_passes = []

(** Economic protocol-specific side information computed by the
     protocol during the validation of a block. Should not include
     information about the evaluation of operations which is handled
     separately by {!operation_metadata}. To be used as an execution
     trace by tools (client, indexer). Not necessary for
     validation. *)


(** Encoding for economic protocol-specific block metadata. *)
(*val block_header_metadata_encoding : block_header_metadata Data_encoding.t *)
type block_header_metadata = Apply_results.block_metadata
let block_header_metadata_encoding =  Apply_results.block_metadata_encoding

(** The economic protocol-specific type of operations. *)
type operation_data = Operation_repr.operation

(** Economic protocol-specific side information computed by the
     protocol during the validation of each operation, to be used
     conjointly with {!block_header_metadata}. *)
type operation_receipt

(** A fully parsed operation. *)
(*
type operation = {
    shell : Operation.shell_header;
    protocol_data : operation_data;
     }
*)
type operation = unit

  (** Encoding for economoic protocol-specific operation data. *)
(*val operation_data_encoding : operation_data Data_encoding.t*)

  (** Encoding for eonomic protocol-specific operation receipts. *)
(*val operation_receipt_encoding : operation_receipt Data_encoding.t*)
let operation_receipt_encoding = () 

(** Encoding that mixes an operation data and its receipt. *)
(*val operation_data_and_receipt_encoding :
    (operation_data * operation_receipt) Data_encoding.t*)

let operation_data_and_receipt_encoding = ()

(*
let operation_data_encoding = Alpha_context.Operation.protocol_data_encoding
*)
let operation_data_encoding = ()

(** [acceptable_passes op] lists the validation passes in which the
     input operation [op] can appear. For instance, it results in
[[0]] if [op] only belongs to the first pass. An answer of [[]]
means that the [op] is ill-formed and cannot be included at
     all in a block. *)
(*val acceptable_passes : operation -> int list*)
let acceptable_passes op = []

(** [relative_position_within_block op1 op2] provides a partial and
     strict order of operations within a block. It is intended to be
     used as an argument to {!List.sort} (and other sorting/ordering
     functions) to arrange a set of operations into a sequence, the
     order of which is valid for the protocol.

     A negative (respectively, positive) results means that [op1]
     should appear before (and, respectively, after) [op2] in a
     block. This function does not provide a total ordering on the
     operations: a result of [0] entails that the protocol does not
     impose any preferences to the order in which [op1] and [op2]
     should be included in a block.

     {b Caveat Emptor!} [relative_position_within_block o1 o2 = 0]
     does NOT imply that [o1] is equal to [o2] in any way.
     Consequently, it {e MUST NOT} be used as a [compare] component of
     an {!Stdlib.Map.OrderedType}, or any such collection which relies
     on a total comparison function. *)

(*val relative_position_within_block : operation -> operation -> int*)
let relative_position_within_block a b = 0

(** A functional state that is transmitted through the steps of a
block validation sequence: it can be created by any of the
[begin_x] functions below, and its final value is produced by
{!finalize_block}. It must retain the current state of the store,
and it can also contain additional information that must be
remembered during the validation process. Said extra content must
however be immutable: validator or baker implementations are
allowed to pause, replay or backtrack throughout validation
steps. *)
type validation_state

(** [begin_partial_application cid ctxt] checks that a block is
well-formed in a given context. This function should run quickly,
as its main use is to reject bad blocks from the chain as early
as possible. The input [ancestor_context] is expected to result
from the application of an ancestor block of the current head
with the same economic protocol. Said ancestor block is also
required to be more recent (i.e., it has a greater level), than
the current head's "last_allowed_fork_level".

The resulting `validation_state` will be used for multi-pass
validation. *)
(*
val begin_partial_application :
    chain_id:Chain_id.t ->
        ancestor_context:Context.t ->
            predecessor_timestamp:Time.t ->
                predecessor_fitness:Fitness.t ->
                    block_header ->
                        validation_state tzresult Lwt.t
                        *)

let begin_partial_application chain_id ancestor_context predecessor_timestamp predecessor_fitness block_header =
    ()

    (** [begin_application chain_id ... bh] defines the first step in a
    block validation sequence. It initializes a validation context
    for validating a block, whose header is [bh]. *)
    (*val begin_application :
        chain_id:Chain_id.t ->
            predecessor_context:Context.t ->
                predecessor_timestamp:Time.t ->
                    predecessor_fitness:Fitness.t ->
                        block_header ->
                            validation_state tzresult Lwt.t*)
let begin_application chain_id predecessor_context predecessor_timestamp predecessor_fitness block_header =
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
