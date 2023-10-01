let is_validator c account =
  Storage.ValidatorSet.get c >>=? fun validator_set ->
  match List.find_opt (fun a -> Account_repr.equal account a) validator_set with
  | Some _ -> Lwt.return (ok true)
  | None -> Lwt.return (ok false)

let add_to_validator_set c account =
  Storage.ValidatorSet.get c >>=? fun validator_set ->
  match List.find_opt (fun v -> Account_repr.equal v account) validator_set with
  | None -> Storage.ValidatorSet.update c (account :: validator_set)
  | Some _ -> Lwt.return (ok c)

let remove_from_validator_set c account =
  Storage.ValidatorSet.get c >>=? fun validator_set ->
  let new_validator_set =
    List.filter
      (fun validator -> not (Account_repr.equal account validator))
      validator_set
  in

  Storage.ValidatorSet.update c new_validator_set

let get_validator_set_size c =
  Storage.ValidatorSet.get c >>=? fun validator_set ->
  Lwt.return (ok (List.length validator_set))

let get_validator_set c = Storage.ValidatorSet.get c

let init c = Storage.ValidatorSet.init c []

let add_initial_list c initial_validators =
  Storage.ValidatorSet.update c initial_validators

let init_with_initial_set c initial_validators =
  init c >>=? fun c -> add_initial_list c initial_validators
