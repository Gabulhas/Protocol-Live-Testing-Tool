open Alpha_context

(*Resultados de quando o apply dá erro, não "apply results" as "aplicar resultados"*)

(*

The apply_results data structure can be thought of as representing the "results of the application" of a set of operations to the current state of the blockchain. It includes information about the operations that were applied, refused, or delayed during the application process, as well as information about the updated state of the blockchain and any errors or warnings that occurred during the application proces
 *)
let trace_encoding = make_trace_encoding error_encoding

type block_metadata = {
  level : Level.compat_t;
  block_hash : Proof_of_work.H.t;
  block_timestamp : Time.t;
  block_nonce : Int64.t;
  block_target : Z.t;
  balance_updates : Receipt.balance_updates;
}

type apply_results = {
  block_metadata : block_metadata;
  applied_operations : operation list;
  refused_operations : operation list;
  branch_refused_operations : operation list;
  branch_delayed_operations : operation list;
  balance_updates : Receipt.balance_updates;
}

type successful_manager_operation_result =
  | Reveal_result
  | Transaction_result of {balance_updates : Receipt.balance_updates}
  | Coinbase_result of {balance_updates : Receipt.balance_updates}

type manager_operation_result =
  | Applied of successful_manager_operation_result
  | Failed of error trace

type operation_result =
  | Manager_operation_result of {
      operation_result : manager_operation_result;
      nonce : int;
    }

let block_metadata_encoding =
  let open Data_encoding in
  conv
    (fun {
           level;
           block_hash;
           block_timestamp;
           block_nonce;
           block_target;
           balance_updates;
         } ->
      ( level,
        block_hash,
        block_timestamp,
        block_nonce,
        block_target,
        balance_updates ))
    (fun ( level,
           block_hash,
           block_timestamp,
           block_nonce,
           block_target,
           balance_updates ) ->
      {
        level;
        block_hash;
        block_timestamp;
        block_nonce;
        block_target;
        balance_updates;
      })
    (obj6
       (req "level" Level.compat_encoding)
       (req "block_hash" Proof_of_work.H.encoding)
       (req "block_timestamp" Time.encoding)
       (req "block_nonce" int64)
       (req "block_target" z)
       (req "balance_updates" Receipt.balance_updates_encoding))

let apply_results_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_metadata;
           applied_operations;
           refused_operations;
           branch_refused_operations;
           branch_delayed_operations;
           balance_updates;
         } ->
      ( block_metadata,
        applied_operations,
        refused_operations,
        branch_refused_operations,
        branch_delayed_operations,
        balance_updates ))
    (fun ( block_metadata,
           applied_operations,
           refused_operations,
           branch_refused_operations,
           branch_delayed_operations,
           balance_updates ) ->
      {
        block_metadata;
        applied_operations;
        refused_operations;
        branch_refused_operations;
        branch_delayed_operations;
        balance_updates;
      })
    (obj6
       (req "block_metadata" block_metadata_encoding)
       (req "applied_operations" (list operation_encoding))
       (req "refused_operations" (list operation_encoding))
       (req "branch_refused_operations" (list operation_encoding))
       (req "branch_delayed_operations" (list operation_encoding))
       (req "balance_updates" Receipt.balance_updates_encoding))

let successful_manager_operation_result_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Reveal_result"
        unit
        (function Reveal_result -> Some () | _ -> None)
        (fun () -> Reveal_result);
      case
        (Tag 1)
        ~title:"Transaction_result"
        (obj1 (req "balance_updates" Receipt.balance_updates_encoding))
        (function
          | Transaction_result {balance_updates} -> Some balance_updates
          | _ -> None)
        (fun balance_updates -> Transaction_result {balance_updates});
      case
        (Tag 2)
        ~title:"Coinbase_result"
        (obj1 (req "balance_updates" Receipt.balance_updates_encoding))
        (function
          | Coinbase_result {balance_updates} -> Some balance_updates
          | _ -> None)
        (fun balance_updates -> Coinbase_result {balance_updates});
    ]

let manager_operation_result_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Applied"
        successful_manager_operation_result_encoding
        (function Applied t -> Some t | _ -> None)
        (fun t -> Applied t);
      case
        (Tag 1)
        ~title:"Failed"
        (obj1 (req "error" trace_encoding))
        (function Failed t -> Some t | _ -> None)
        (fun t -> Failed t);
    ]

let operation_result_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"manager_operation_encoding"
        (obj2
           (req "operation_result" manager_operation_result_encoding)
           (req "nonce" uint8))
        (function
          | Manager_operation_result {operation_result; nonce}
            ->
              Some (operation_result, nonce))
        (fun (operation_result, nonce) ->
          Manager_operation_result {operation_result; nonce});
    ]

