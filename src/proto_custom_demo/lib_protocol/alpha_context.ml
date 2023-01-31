type t = Raw_context.t

type context = t

module type BASIC_DATA = sig
  type t

  include Compare.S with type t := t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

module Tez = Tez_repr
include Operation_repr
module Operation = Operation_repr
module Block_header = Block_header_repr

type public_key = Signature.Public_key.t

type public_key_hash = Signature.Public_key_hash.t

type signature = Signature.t

module Raw_context = Raw_context
module Receipt = Receipt_repr

module Fitness = struct
  type raw = Fitness.t

  include Fitness_repr
end

module Account = struct
  include Account_repr
  include Account_storage
end

let description = Raw_context.description

module Gas = struct
  include Gas_limit_repr

  (*
  type error += Gas_limit_too_high = Raw_context.Gas_limit_too_high

  type error += Block_quota_exceeded = Raw_context.Block_quota_exceeded

  type error +=
    | Operation_quota_exceeded = Raw_context.Operation_quota_exceeded
  let check_limit = Raw_context.check_gas_limit

  let set_limit = Raw_context.set_gas_limit

  let set_unlimited = Raw_context.set_gas_unlimited
*)

  let consume = Raw_context.consume_gas

  let check_enough = Raw_context.check_enough_gas

  (*
  let level = Raw_context.gas_level

  let consumed = Raw_context.gas_consumed

  let block_level = Raw_context.block_gas_level
*)
  (* Necessary to inject costs for Storage_costs into Gas.cost *)
  let cost_of_repr cost = cost
end

module Raw_level = Raw_level_repr

module Level = struct
  include Level_repr
end

module Proof_of_work = Proof_of_work

let prepare_first_block = Init_storage.prepare_first_block

let prepare = Init_storage.prepare

let constants = Raw_context.constants

let level = Raw_context.level

let timestamp = Raw_context.timestamp


module Constants = struct
  include Constants_repr

  let all ctxt = all_of_parametric (constants ctxt)
end

let int64_to_bytes i =
  let b = Bytes.make 8 '\000' in
  TzEndian.set_int64 b 0 i ;
  b

let fitness_from_level level =
  [
    Bytes.of_string "1";
    Bytes.of_string "\000";
    Bytes.of_string "\000";
    Bytes.of_string "\000";
    int64_to_bytes level;
  ]

let finalize ?commit_message:message (c : context) : Updater.validation_result =
  let fitness = fitness_from_level Int64.(succ (of_int32 (level c))) in
  let context = Raw_context.context c in
  let first_level = Raw_context.first_level c in
  Logging.log
    Notice
    "finalize: Message %s, Fitness/Level %s, Level %s"
    (match message with Some a -> a | None -> "NONE")
    (fitness |> List.map Bytes.to_string |> String.concat ",")
    (Int32.to_string (level c)) ;

  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl = 60;
    (*TODO fix this*)
    last_allowed_fork_level = first_level;
  }
