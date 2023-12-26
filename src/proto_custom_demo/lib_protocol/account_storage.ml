type error +=
  | Balance_too_low of Account_repr.t * Tez_repr.t * Tez_repr.t
  | Counter_in_the_past of Account_repr.t * Z.t * Z.t
  | Counter_in_the_future of Account_repr.t * Z.t * Z.t
  | Empty_account of Signature.Public_key_hash.t
  | Inconsistent_hash of
      Signature.Public_key.t
      * Signature.Public_key_hash.t
      * Signature.Public_key_hash.t
  | Inconsistent_public_key of Signature.Public_key.t * Signature.Public_key.t
  | Failure of string
  | Previously_revealed_key of Account_repr.t (* `Permanent *)
  | Unrevealed_manager_key of Account_repr.t

let () =
  register_error_kind
    `Temporary
    ~id:"account.balance_too_low"
    ~title:"Balance too low"
    ~description:"An operation tried to spend more tokens than the account has"
    ~pp:(fun ppf (c, b, a) ->
      Format.fprintf
        ppf
        "Balance of Account %a too low (%a) to spend %a"
        Account_repr.pp
        c
        Tez_repr.pp
        b
        Tez_repr.pp
        a)
    Data_encoding.(
      obj3
        (req "account" Account_repr.encoding)
        (req "balance" Tez_repr.encoding)
        (req "amount" Tez_repr.encoding))
    (function Balance_too_low (c, b, a) -> Some (c, b, a) | _ -> None)
    (fun (c, b, a) -> Balance_too_low (c, b, a)) ;

  register_error_kind
    `Temporary
    ~id:"account.counter_in_the_future"
    ~title:"Invalid counter (not yet reached) in a manager operation"
    ~description:"An operation assumed a account counter in the future"
    ~pp:(fun ppf (account, exp, found) ->
      Format.fprintf
        ppf
        "Counter %a not yet reached for account %a (expected %a)"
        Z.pp_print
        found
        Account_repr.pp
        account
        Z.pp_print
        exp)
    Data_encoding.(
      obj3
        (req "account" Account_repr.encoding)
        (req "expected" z)
        (req "found" z))
    (function Counter_in_the_future (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_future (c, x, y)) ;

  register_error_kind
    `Branch
    ~id:"account.counter_in_the_past"
    ~title:"Invalid counter (already used) in a manager operation"
    ~description:"An operation assumed a account counter in the past"
    ~pp:(fun ppf (account, exp, found) ->
      Format.fprintf
        ppf
        "Counter %a already used for account %a (expected %a)"
        Z.pp_print
        found
        Account_repr.pp
        account
        Z.pp_print
        exp)
    Data_encoding.(
      obj3
        (req "account" Account_repr.encoding)
        (req "expected" z)
        (req "found" z))
    (function Counter_in_the_past (c, x, y) -> Some (c, x, y) | _ -> None)
    (fun (c, x, y) -> Counter_in_the_past (c, x, y)) ;

  register_error_kind
    `Permanent
    ~id:"account.manager.inconsistent_hash"
    ~title:"Inconsistent public key hash"
    ~description:
      "A revealed manager public key is inconsistent with the announced hash"
    ~pp:(fun ppf (k, eh, ph) ->
      Format.fprintf
        ppf
        "The hash of the manager public key %s is not %a as announced but %a"
        (Signature.Public_key.to_b58check k)
        Signature.Public_key_hash.pp
        ph
        Signature.Public_key_hash.pp
        eh)
    Data_encoding.(
      obj3
        (req "public_key" Signature.Public_key.encoding)
        (req "expected_hash" Signature.Public_key_hash.encoding)
        (req "provided_hash" Signature.Public_key_hash.encoding))
    (function Inconsistent_hash (k, eh, ph) -> Some (k, eh, ph) | _ -> None)
    (fun (k, eh, ph) -> Inconsistent_hash (k, eh, ph)) ;

  register_error_kind
    `Permanent
    ~id:"account.manager.inconsistent_public_key"
    ~title:"Inconsistent public key"
    ~description:
      "A provided manager public key is different with the public key stored \
       in the account"
    ~pp:(fun ppf (eh, ph) ->
      Format.fprintf
        ppf
        "Expected manager public key %s but %s was provided"
        (Signature.Public_key.to_b58check ph)
        (Signature.Public_key.to_b58check eh))
    Data_encoding.(
      obj2
        (req "public_key" Signature.Public_key.encoding)
        (req "expected_public_key" Signature.Public_key.encoding))
    (function Inconsistent_public_key (eh, ph) -> Some (eh, ph) | _ -> None)
    (fun (eh, ph) -> Inconsistent_public_key (eh, ph)) ;

  register_error_kind
    `Branch
    ~id:"account.unrevealed_key"
    ~title:"Manager operation precedes key revelation"
    ~description:
      "One tried to apply a manager operation without revealing the manager \
       public key"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Unrevealed manager key for account %a."
        Account_repr.pp
        s)
    Data_encoding.(obj1 (req "account" Account_repr.encoding))
    (function Unrevealed_manager_key s -> Some s | _ -> None)
    (fun s -> Unrevealed_manager_key s) ;

  register_error_kind
    `Branch
    ~id:"account.previously_revealed_key"
    ~title:"Manager operation already revealed"
    ~description:"One tried to revealed twice a manager public key"
    ~pp:(fun ppf s ->
      Format.fprintf
        ppf
        "Previously revealed manager key for account %a."
        Account_repr.pp
        s)
    Data_encoding.(obj1 (req "account" Account_repr.encoding))
    (function Previously_revealed_key s -> Some s | _ -> None)
    (fun s -> Previously_revealed_key s)

let failwith msg = fail (Failure msg)

let init c = Storage.Account.Global_counter.init c Z.zero

let create_account c manager ~balance =
  let account : Account_repr.t = manager in
  Storage.Account.Global_counter.get c >>=? fun counter ->
  Storage.Account.Counter.init c account counter >>=? fun c ->
  Storage.Account.Balance.init c account balance >>=? fun c ->
  Storage.Account.Manager.init c account (Manager_repr.Hash manager)

let exists c manager =
  Storage.Account.Balance.find c manager >>=? function
  | None -> return_false
  | Some _ -> return_true

let list c = Storage.Account.list c

let initialize_if_needed ctxt account =
  Storage.Account.Balance.find ctxt account >>=? function
  | None -> create_account ctxt account ~balance:Tez_repr.zero
  | Some _ -> return ctxt

let get_balance_exists_account c account =
  Storage.Account.Balance.find c account >>=? function
  | None -> return Tez_repr.zero
  | Some v -> return v

let update_balance ctxt account f amount =
  Storage.Account.Balance.get ctxt account >>=? fun balance ->
  f balance amount >>?= fun new_balance ->
  Storage.Account.Balance.update ctxt account new_balance

let increase_balance ctxt account amount =
  update_balance ctxt account Tez_repr.( +? ) amount

let decrease_balance ctxt account amount =
  update_balance ctxt account Tez_repr.( -? ) amount

let increment_counter c manager =
  let account = manager in
  initialize_if_needed c manager >>=? fun c ->
  Storage.Account.Global_counter.get c >>=? fun global_counter ->
  Storage.Account.Global_counter.update c (Z.succ global_counter) >>=? fun c ->
  Storage.Account.Counter.get c account >>=? fun account_counter ->
  Storage.Account.Counter.update c account (Z.succ account_counter)

let check_counter_increment c manager counter =
  let account = manager in
  initialize_if_needed c manager >>=? fun c ->
  Storage.Account.Counter.get c account >>=? fun account_counter ->
  let expected = Z.succ account_counter in
  if Compare.Z.(expected = counter) then return_unit else return_unit
(*TODO FIX LATER
  else if Compare.Z.(expected > counter) then
    fail (Counter_in_the_past (account, expected, counter))
  else fail (Counter_in_the_future (account, expected, counter))
*)

let get_counter c account =
  initialize_if_needed c account >>=? fun c ->
  Storage.Account.Counter.get c account

let get_new_counter c account =
  initialize_if_needed c account >>=? fun c ->
  Storage.Account.Counter.get c account >>=? fun counter ->
  return (Z.succ counter)

let debit_account ctxt account amount =
  initialize_if_needed ctxt account >>=? fun ctxt ->
  get_balance_exists_account ctxt account >>=? fun current_balance ->
  let open Tez_repr in
  if current_balance < amount then
    fail (Balance_too_low (account, current_balance, amount))
  else decrease_balance ctxt account amount

let credit_account ctxt account amount =
  initialize_if_needed ctxt account >>=? fun ctxt ->
  increase_balance ctxt account amount

let reveal_manager_key c manager public_key =
  let account = manager in
  initialize_if_needed c manager >>=? fun c ->
  Storage.Account.Manager.get c account >>=? function
  | Public_key _ -> fail (Previously_revealed_key account)
  | Hash v ->
      let actual_hash = Signature.Public_key.hash public_key in
      if Signature.Public_key_hash.equal actual_hash v then
        let v = Manager_repr.Public_key public_key in
        Storage.Account.Manager.update c account v
      else fail (Inconsistent_hash (public_key, v, actual_hash))

let get_manager_key c manager =
  let account = manager in
  initialize_if_needed c manager >>=? fun c ->
  Storage.Account.Manager.find c account >>=? function
  | None -> failwith "get_manager_key"
  | Some (Manager_repr.Public_key v) -> return v
  | Some (Manager_repr.Hash _) -> fail (Unrevealed_manager_key account)

let is_revealed c manager =
  initialize_if_needed c manager >>=? fun c ->
  Storage.Account.Manager.find c manager >>=? function
  | None -> return_false
  | Some (Manager_repr.Hash _) -> return_false
  | Some _ -> return_true
