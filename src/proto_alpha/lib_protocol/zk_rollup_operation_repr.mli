(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(** A ZK rollup L2 operation has two parts: a transparent header and
    an opaque payload.
    The header is made up by:
    {ul
      {li An [op_code] in the range \[0, nb_ops)}
      {li [price = (ticket_hash, amount)] where [ticket_hash] is used as
        a ticket identifier, and [amount] positive if the operation transfers
        tickets from L1 to L2, negative if it does so from L2 to L1, and zero
        when no transfer is done between layers}
      {li [l1_dst] is the public key hash of the implicit account that will
        be credited with the withdrawal generated by this operation, if any}
      {li [rollup_id] is the address of the rollup this operation targets}
    }

    This type represents the L1's view of L2 operations. It's important
    to remember that this is only used for public operations, as the
    protocol isn't aware of private ones.
*)
type t = {
  op_code : int;
  price : Ticket_hash_repr.t * Z.t;
  l1_dst : Signature.Public_key_hash.t;
  rollup_id : Zk_rollup_repr.t;
  payload : Zk_rollup_scalar.t array;
}

val encoding : t Data_encoding.t

(** Special encoding needed to feed L2 operations to the Plonk verifier *)
val to_scalar_array : t -> Zk_rollup_scalar.t array