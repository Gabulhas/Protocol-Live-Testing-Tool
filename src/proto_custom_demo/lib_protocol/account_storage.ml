let create_implicit c manager ~balance =
  let contract = Contract_repr.Implicit manager in
  Storage.Contract.Global_counter.get c >>=? fun counter ->
  Storage.Contract.Counter.init c contract counter >>=? fun c ->
  Storage.Contract.Spendable_balance.init c contract balance >>=? fun c ->
  Contract_manager_storage.init c contract (Manager_repr.Hash manager)

let allocated c contract = Storage.Contract.Spendable_balance.mem c contract

let exists c contract =
  match contract with
  | Contract_repr.Implicit _ -> Lwt.return_true
  | Originated _ -> allocated c contract

let must_exist c contract =
  exists c contract >>= function
  | true -> return_unit
  | false -> fail (Non_existing_contract contract)

let must_be_allocated c contract =
  allocated c contract >>= function
  | true -> return_unit
  | false -> (fail (Empty_implicit_contract pkh))

let list c = Storage.Contract.list c

let check_counter_increment c manager counter =
  let contract = Contract_repr.Implicit manager in
  Storage.Contract.Counter.get c contract >>=? fun contract_counter ->
  let expected = Manager_counter_repr.succ contract_counter in
  if Manager_counter_repr.(expected = counter) then return_unit
  else if Manager_counter_repr.(expected > counter) then
    fail (Counter_in_the_past {contract; expected; found = counter})
  else fail (Counter_in_the_future {contract; expected; found = counter})

let increment_counter c manager =
  let contract = Contract_repr.Implicit manager in
  Storage.Contract.Global_counter.get c >>=? fun global_counter ->
  Storage.Contract.Global_counter.update
    c
    (Manager_counter_repr.succ global_counter)
  >>=? fun c ->
  Storage.Contract.Counter.get c contract >>=? fun contract_counter ->
  Storage.Contract.Counter.update
    c
    contract
    (Manager_counter_repr.succ contract_counter)


let get_counter c manager =
  let contract = Contract_repr.Implicit manager in
  Storage.Contract.Counter.find c contract >>=? function
  | None -> (
      match contract with
      | Contract_repr.Implicit _ -> Storage.Contract.Global_counter.get c
      | Originated _ -> failwith "get_counter")
  | Some v -> return v

let get_balance c contract =
  Storage.Contract.Spendable_balance.find c contract >>=? function
  | None -> (
      match contract with
      | Implicit _ -> return Tez_repr.zero
      | Originated _ -> failwith "get_balance")
  | Some v -> return v


let check_allocated_and_get_balance c pkh =
  let open Lwt_tzresult_syntax in
  let* balance_opt =
    Storage.Contract.Spendable_balance.find c (Contract_repr.Implicit pkh)
  in
  match balance_opt with
  | None -> fail (Empty_implicit_contract pkh)
  | Some balance -> return balance

let spend_from_balance contract balance amount =
  record_trace
    (Balance_too_low (contract, balance, amount))
    Tez_repr.(balance -? amount)


let init c =
  Storage.Contract.Global_counter.init c Manager_counter_repr.init >>=? fun c ->
  Lazy_storage_diff.init c

let update_balance ctxt contract f amount =
  Storage.Contract.Spendable_balance.get ctxt contract >>=? fun balance ->
  f balance amount >>?= fun new_balance ->
  Storage.Contract.Spendable_balance.update ctxt contract new_balance


(** Indicate whether the given implicit contract should avoid deletion
    when it is emptied. *)
let should_keep_empty_implicit_contract ctxt contract =
  let open Lwt_tzresult_syntax in
  let* has_frozen_bonds = has_frozen_bonds ctxt contract in
  if has_frozen_bonds then return_true
  else
    (* full balance of contract is zero. *)
    Contract_delegate_storage.find ctxt contract >>=? function
    | Some _ ->
        (* Here, we know that the contract delegates to itself.
           Indeed, it does not delegate to a different one, because
           the balance of such contracts cannot be zero (see
           {!spend_only_call_from_token}), hence the stake of such
           contracts cannot be zero either. *)
        return_true
    | None ->
        (* Delete empty implicit contract. *)
        return_false

