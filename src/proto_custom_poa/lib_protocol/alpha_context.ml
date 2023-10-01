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

let current_context c = Raw_context.context c

let description : context Storage_description.desc_with_path =
  Raw_context.description

let prepare = Raw_context.prepare

let prepare_first_block = Init_storage.prepare_first_block

module Parameters = Parameters_repr
module Receipt = Receipt_repr
module Cache = Cache_repr

let constants = Raw_context.constants

let level = Raw_context.level

let timestamp = Raw_context.timestamp

(*
let prepare_first_block = 

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =
*)

let finalize ?commit_message:message c fitness =
  let context = Raw_context.context c in
  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl = 60;
    last_allowed_fork_level = 0l;
  }

module Account = struct
  include Account_repr
  include Account_storage
end

module Constants = struct
  include Constants_repr

  let all ctxt = all_of_parametric (constants ctxt)
end
