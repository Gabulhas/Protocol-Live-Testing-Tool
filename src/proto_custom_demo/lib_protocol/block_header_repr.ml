(*
type shell_header = {
  level : Int32.t;  (** Height of the block, from the genesis block. *)
  proto_level : int;
      (** Number (uint8) of protocol changes since genesis modulo 256. *)
  predecessor : Block_hash.t;  (** Hash of the preceding block. *)
  timestamp : Time.t;
      (** Timestamp at which the block is claimed to have been created. *)
  validation_passes : int;
      (** Number (uint8) of validation passes (also number of lists of operations). *)
  operations_hash : Operation_list_list_hash.t;
      (** Hash of the list of lists (actually root hashes of merkle trees)
          of operations included in the block. There is one list of
          operations per validation pass. *)
  fitness : Bytes.t list;
      (** A sequence of sequences of unsigned bytes, ordered by length and
          then lexicographically. It represents the claimed fitness of the
          chain ending in this block. *)
  context : Context_hash.t;
      (** Hash of the state of the context after application of this block. *)
}

 *)
type contents = {

    time: time_repr.t;
    nBits: nBits_repr.t;
    nonce: Nonce_repr.t
}


type t = {
    shell_header: Block_header.shell_header;
    protocol_part: contents
 }
