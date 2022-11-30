(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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
type missing_key_kind = Get | Set | Del | Copy

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list


type error += Storage_error of storage_error


module Int_set = Set.Make (Compare.Int)

type t = {
  context: Context.t ;
  timestamp: Time.t ;
  fitness: Int64.t ;
  fees: Tez_repr.t ;
  rewards: Tez_repr.t ;
  internal_nonce: int ;
  internal_nonces_used: Int_set.t ;
  (*TODO: add PoW specific stuff*)
}

type context = t

type root_context = t

type root = t


let current_context               ctxt = ctxt.context
let current_timestamp             ctxt = ctxt.timestamp
let current_fitness               ctxt = ctxt.fitness
let current_fees                  ctxt = ctxt.fees
let current_rewards               ctxt = ctxt.rewards
let current_internal_nonce        ctxt = ctxt.internal_nonce
let current_internal_nonces_used  ctxt = ctxt.internal_nonces_used

(*This is used to do CRUD stuff with reprs in the chain (in the Context.t)


VVVVVVVVVVVVVVVVVVVVVv


 *)


(* Generic context ********************************************************)

type key = string list

type value = bytes

type tree = Context.tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree
