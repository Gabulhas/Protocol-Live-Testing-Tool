type t = Raw_context.t

type context = t

module type BASIC_DATA = sig
  type t

  include Compare.S with type t := t

  val encoding : t Data_encoding.t

  val pp : Format.formatter -> t -> unit
end

module Block_header = Block_header_repr

module Parameters = Parameters_repr

module Cache = Cache_repr

let description = Raw_context.description

let prepare_first_block = 

let prepare ctxt ~level ~predecessor_timestamp ~timestamp =

let finalize ?commit_message:message c fitness =
  let context = Raw_context.context c in
  {
    Updater.context;
    fitness;
    message;
    max_operations_ttl = (Raw_context.constants c).max_operations_time_to_live;
    last_allowed_fork_level =
      Raw_level.to_int32 @@ Level.last_allowed_fork_level c;
  }


