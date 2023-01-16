(*TODO: Change these values*)
(*TODO: remove non-Nakamoto consensus stuff*)
let version_number = "\001"

let nonce_length = 32

let max_anon_ops_per_block = 132

let max_proposals_per_delegate = 20

let max_operation_data_length = 32 * 1024 (* 32kB *)



type fixed = {
  nonce_length : int;
  max_anon_ops_per_block : int;
  max_operation_data_length : int;
  max_proposals_per_delegate : int;
}

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun c ->
      ( 
        c.nonce_length,
        c.max_anon_ops_per_block,
        c.max_operation_data_length,
        c.max_proposals_per_delegate ))
    (fun (
           nonce_length,
           max_anon_ops_per_block,
           max_operation_data_length,
           max_proposals_per_delegate ) ->
      {
        nonce_length;
        max_anon_ops_per_block;
        max_operation_data_length;
        max_proposals_per_delegate;
      })
    (obj4
       (req "nonce_length" uint8)
       (req "max_anon_ops_per_block" uint8)
       (req "max_operation_data_length" int31)
       (req "max_proposals_per_delegate" uint8))

let fixed =
  {
    nonce_length;
    max_anon_ops_per_block;
    max_operation_data_length;
    max_proposals_per_delegate;
  }


type parametric =
    {
        block_time: Time_repr.t;
        initial_target: Target_repr.t;
        difficulty_adjust_epoch_size: Int32.t;

        halving_epoch_size: Int32.t;
        reward_multiplier: Tez_repr.t;

        (*TODO:
            This is the reward formula from Bitcoin:
                halving_epoch_size is the the number of blocks per halving epoch (210k)
                reward_multiplier is the reward multiplier/initial value (50BTC)
                height is the current height of a block, which comes from the shell header, not the protocol header
                the formula is the following:

                    reward_multiplier / (2 ^ intpart(height / halving_epoch_size))
        *)
  }

let parametric_encoding =
  let open Data_encoding in
  conv
    (fun {block_time; initial_target; difficulty_adjust_epoch_size; halving_epoch_size; reward_multiplier} ->
       (block_time, initial_target, difficulty_adjust_epoch_size, halving_epoch_size, reward_multiplier))
    (fun (block_time, initial_target, difficulty_adjust_epoch_size, halving_epoch_size, reward_multiplier) ->
       {block_time; initial_target; difficulty_adjust_epoch_size; halving_epoch_size; reward_multiplier})
    (obj5
       (req "block_time" Time_repr.encoding)
       (req "initial_target" Target_repr.encoding)
       (req "difficulty_adjust_epoch_size" int32)
       (req "halving_epoch_size" int32)
       (req "reward_multiplier" Tez_repr.encoding))

type t = {fixed : fixed; parametric : parametric}

let encoding =
  let open Data_encoding in
  conv
    (fun {fixed; parametric} -> (fixed, parametric))
    (fun (fixed, parametric) -> {fixed; parametric})
    (merge_objs fixed_encoding parametric_encoding)

