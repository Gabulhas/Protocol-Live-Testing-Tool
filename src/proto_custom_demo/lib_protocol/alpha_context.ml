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

