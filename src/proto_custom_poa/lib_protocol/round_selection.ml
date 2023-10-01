let validator_hash_prefix = "\023\157\178\215"

module ValidatorHash = struct
  let prefix = "2B4PL"

  let encoded_size = 55

  module H =
    Blake2B.Make
      (Base58)
      (struct
        let name = "validator_hash"

        let title = "Hash for sorting validators for block baking"

        let b58check_prefix = validator_hash_prefix

        let size = None
      end)

  include H

  let () = Base58.check_encoded_prefix b58check_encoding prefix encoded_size
end

let hash_validator_and_round (validator : Account_repr.t) round =
  ValidatorHash.hash_bytes
    [
      Account_repr.to_bytes validator;
      round |> Int32.to_string |> Bytes.of_string;
    ]

let hash_validator_and_round_b58_check validator round =
  ValidatorHash.to_b58check @@ hash_validator_and_round validator round

let sort_validators_by_hash validators round =
  List.sort
    ValidatorHash.compare
    (List.map
       (fun validator -> hash_validator_and_round validator round)
       validators)

let skips_to_order skips num_validators =
  Int64.to_int (Int64.rem skips (Int64.of_int num_validators))

let is_validator_the_current_proposer validator validators round skips =
  let this_validator_round_hash = hash_validator_and_round validator round in

  match
    List.nth
      (sort_validators_by_hash validators round)
      (skips_to_order skips (List.length validators))
  with
  | None -> false
  | Some a -> ValidatorHash.equal a this_validator_round_hash
