(*

Abstract communication between top and bottom, that is, between the consensus part of the protocol and the storage of it's information
ledger is a KV struct
 *)

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

(*
let finalize ?commit_message:message c =
  let fitness = Fitness.from_int64 (Fitness.current c) in
  let context = Raw_context.context c in
  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl = 60;
    last_allowed_fork_level = 0l
  }
*)





let constants = Raw_context.constants
let level = Raw_context.level
let timestamp = Raw_context.timestamp

(* TODO: add the remaining ones

let _timestamp = Raw_context.timestamp

let _internal_nonce = Raw_context.internal_nonce

let _internal_nonces_used = Raw_context.internal_nonces_used

let remaining_operation_gas = Raw_context.remaining_operation_gas

let unlimited_operation_gas = Raw_context.unlimited_operation_gas
*)

module Constants = struct
  include Constants_repr

  let all ctxt = all_of_parametric (constants ctxt)
end


let finalize ?commit_message:message (c:context): Updater.validation_result =
    let fitness = Raw_context.level c in
    let context = Raw_context.context c in
    {
        Updater.context;
        fitness=[(Data_encoding.Binary.to_bytes_exn Data_encoding.int32 fitness)];
        message;
        max_operations_ttl=60;
        (*TODO fix this*)
        last_allowed_fork_level=0l;
  }

