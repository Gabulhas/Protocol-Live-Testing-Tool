let version_number = "\001"

let nonce_length = 32

let max_operation_data_length = 32 * 1024 (* 32kB *)

type fixed = {nonce_length : int; max_operation_data_length : int}

let fixed_encoding =
  let open Data_encoding in
  conv
    (fun c -> (c.nonce_length, c.max_operation_data_length))
    (fun (nonce_length, max_operation_data_length) ->
      {nonce_length; max_operation_data_length})
    (obj2 (req "nonce_length" uint8) (req "max_operation_data_length" int31))

let fixed = {nonce_length; max_operation_data_length}

(*Also add block operation count*)
type parametric = {
  block_time : Time_repr.t;
  initial_target : Target_repr.t;
  difficulty_adjust_epoch_size : Int32.t;
  halving_epoch_size : Int32.t;
  reward_multiplier : Tez_repr.t;
}

let parametric_encoding =
  let open Data_encoding in
  conv
    (fun {
           block_time;
           initial_target;
           difficulty_adjust_epoch_size;
           halving_epoch_size;
           reward_multiplier;
         } ->
      ( block_time,
        initial_target,
        difficulty_adjust_epoch_size,
        halving_epoch_size,
        reward_multiplier ))
    (fun ( block_time,
           initial_target,
           difficulty_adjust_epoch_size,
           halving_epoch_size,
           reward_multiplier ) ->
      {
        block_time;
        initial_target;
        difficulty_adjust_epoch_size;
        halving_epoch_size;
        reward_multiplier;
      })
    (obj5
       (req "block_time" Time_repr.encoding)
       (req "initial_target" Target_repr.encoding)
       (req "difficulty_adjust_epoch_size" int32)
       (req "halving_epoch_size" int32)
       (req "reward_multiplier" Tez_repr.encoding))

type t = {fixed : fixed; parametric : parametric}

let all_of_parametric parametric = {fixed; parametric}

let encoding =
  let open Data_encoding in
  conv
    (fun {fixed; parametric} -> (fixed, parametric))
    (fun (fixed, parametric) -> {fixed; parametric})
    (merge_objs fixed_encoding parametric_encoding)
