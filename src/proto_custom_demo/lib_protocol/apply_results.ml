open Alpha_context

(*Resultados de quando o apply dá erro, não "apply results" as "aplicar resultados"*)

(*

The apply_results data structure can be thought of as representing the "results of the application" of a set of operations to the current state of the blockchain. It includes information about the operations that were applied, refused, or delayed during the application process, as well as information about the updated state of the blockchain and any errors or warnings that occurred during the application proces
 *)

type block_metadata = {
  level : Level.compat_t;
  level_info : Level.t;
  block_hash : string;
  block_timestamp : Time.t;
  block_nonce : int;
  block_difficulty : int;
  balance_updates : Receipt.balance_updates;
}

open Data_encoding

let block_metadata_encoding =
  obj7
    (req "level" Level.compat_encoding)
    (req "level_info" Level.encoding)
    (req "block_hash" string)
    (req "block_timestamp" Time.encoding)
    (req "block_nonce" int32)
    (req "block_difficulty" int32)
    (req "balance_updates" Receipt.balance_updates_encoding)

type apply_results = {
  block_metadata : block_metadata;
  applied_operations : operation list;
  refused_operations : operation list;
  branch_refused_operations : operation list;
  branch_delayed_operations : operation list;
  balance_updates : Receipt.balance_updates;
}

let apply_results_encoding =
  obj5
    (req "block_metadata" block_metadata_encoding)
    (req "applied_operations" (list Operation.encoding))
    (req "refused_operations" (list Operation.encoding))
    (req "branch_refused_operations" (list Operation.encoding))
    (req "branch_delayed_operations" (list Operation.encoding))

type transaction_result =
  | Applied
  | Backtracked of error trace option
  | Failed of error trace
  | Skipped

type operation_result =
  | TransactionResult of {
      balance_updates : Receipt.balance_updates;
      operation_result : transaction_result;
      internal_operation_results : packed_internal_operation_result list;
    }
