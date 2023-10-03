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

let sort_validators_by_hash_as_hashes validators round =
  List.sort
    ValidatorHash.compare
    (List.map
       (fun validator -> hash_validator_and_round validator round)
       validators)

let sort_validators_by_hash_as_addresses validators round =
  validators
  |> List.map (fun validator ->
         (hash_validator_and_round validator round, validator))
  |> List.sort (fun (a, _) (b, _) -> ValidatorHash.compare a b)
  |> List.map (fun (_, b) -> b)

let skips_to_order skips num_validators =
  Int64.to_int (Int64.rem skips (Int64.of_int num_validators))

let calculate_skips current_timestamp predecessor_timestamp block_time tolerance
    =
  let time_difference = Int64.sub current_timestamp predecessor_timestamp in
  let interval = Int64.add block_time tolerance in
  Int64.div time_difference interval

let expected_validator_address_with_skips validators round skips =
  List.nth
    (sort_validators_by_hash_as_addresses validators round)
    (skips_to_order skips (List.length validators))

let expected_validator_with_skips validators round skips =
  List.nth
    (sort_validators_by_hash_as_hashes validators round)
    (skips_to_order skips (List.length validators))

let is_validator_the_current_proposer validator validators round skips =
  let this_validator_round_hash = hash_validator_and_round validator round in

  match expected_validator_with_skips validators round skips with
  | None -> false
  | Some a -> ValidatorHash.equal a this_validator_round_hash

let get_validator validators round current_timestamp predecessor_timestamp
    block_time tolerance =
  let skips =
    calculate_skips current_timestamp predecessor_timestamp block_time tolerance
  in
  Logging.log
    Logging.Notice
    "GET_VALIDATOR. ROUND %s| C_TIME %s| P_TIME %s"
    (Int32.to_string round)
    (Int64.to_string current_timestamp)
    (Int64.to_string predecessor_timestamp) ;
  Logging.log
    Logging.Notice
    "GET_VALIDATOR. BL_TIME %s| ToL %s| Skips %s"
    (Int64.to_string block_time)
    (Int64.to_string tolerance)
    (Int64.to_string skips) ;

  List.nth
    (sort_validators_by_hash_as_addresses validators round)
    (skips_to_order skips (List.length validators))

let get_block_time_bounds predecessor_timestamp current_timestamp block_time
    tolerance =
  let predecessor_timestamp = Time.to_seconds predecessor_timestamp in
  let current_timestamp = Time.to_seconds current_timestamp in
  let block_time = Time.to_seconds block_time in
  let tolerance = Time.to_seconds tolerance in

  let skips =
    calculate_skips current_timestamp predecessor_timestamp block_time tolerance
  in

  let expected_timestamp = Int64.mul block_time (Int64.add 1L skips) in
  let lower_bound = Int64.sub expected_timestamp tolerance in
  let upper_bound = Int64.add expected_timestamp tolerance in

  (lower_bound, expected_timestamp, upper_bound)
