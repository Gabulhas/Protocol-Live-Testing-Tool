let version_number = "\001"

type parametric = {
  initial_validator_set : Account_repr.t list;
      [@encoding Account_repr.authority_list_encoding]
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
