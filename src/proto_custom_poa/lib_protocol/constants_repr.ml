let version_number = "\001"

type validator_pair = {
  address : Account_repr.t;
  public_key : Signature.Public_key.t;
}
[@@deriving encoding]

type parametric = {
  initial_validator_set : validator_pair list;
  tolerance : Time.t;
  block_time : Time.t;
  validator_initial_reward : Tez_repr.t;
}
[@@deriving encoding]

type fixed = {
  version_number : string;
      (*Constants that aren't parametric/are generic to the protocol*)
}
[@@deriving encoding]

let fixed = {version_number}

type t = {fixed : fixed; parametric : parametric} [@@deriving encoding]

let all_of_parametric parametric = {fixed; parametric}
