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

module Operation = Transaction_repr

module Block_header = Block_header_repr

type public_key = Signature.Public_key.t

type public_key_hash = Signature.Public_key_hash.t

type signature = Signature.t

(*
module Constants = struct
    include Constants_repr
  include Constants_storage
  module Parametric = Constants_parametric_repr

  let round_durations ctxt = Raw_context.round_durations ctxt

  let all ctxt = all_of_parametric (parametric ctxt)
end
*)

module Receipt = Receipt_repr

module Fitness = struct
    type raw = Fitness.t

  include Fitness_repr
end
