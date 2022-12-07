let create_account c manager ~balance =
  let account : Account_repr.t = manager in
  Storage.Account.Global_counter.get c >>=? fun counter ->
  Storage.Account.Counter.init c account counter >>=? fun c ->
  Storage.Account.Balance.init c account balance >>=? fun c ->
  Storage.Account.Manager.init c account (Manager_repr.Hash manager)

(*REMOVE: only makes sense in a context that smart contacts exist*)
let exists _ _ = Lwt.return_true

let list c = Storage.Account.list c

let get_balance c account =
  Storage.Account.Balance.find c account >>=? function
  | None -> return Tez_repr.zero
  | Some v -> return v

let update_balance ctxt contract f amount =
  Storage.Account.Balance.get ctxt contract >>=? fun balance ->
  f balance amount >>?= fun new_balance ->
  Storage.Account.Balance.update ctxt contract new_balance

let increase_balance ctxt contract amount =
  update_balance ctxt contract Tez_repr.( +? ) amount

let decrease_balance ctxt contract amount =
  update_balance ctxt contract Tez_repr.( -? ) amount
