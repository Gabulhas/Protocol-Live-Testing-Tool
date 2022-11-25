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

module type T = sig

  type t
  type context = t

  val mem: context -> key -> bool Lwt.t
  val dir_mem: context -> key -> bool Lwt.t
  val get: context -> key -> value tzresult Lwt.t
  val get_option: context -> key -> value option Lwt.t
  val init: context -> key -> value -> context tzresult Lwt.t
  val set: context -> key -> value -> context tzresult Lwt.t
  val init_set: context -> key -> value -> context Lwt.t
  val set_option: context -> key -> value option -> context Lwt.t
  val delete: context -> key -> context tzresult Lwt.t
  val remove: context -> key -> context Lwt.t
  val remove_rec: context -> key -> context Lwt.t
  val copy: context -> from:key -> to_:key -> context tzresult Lwt.t

  val fold:
    context -> key -> init:'a ->
    f:([ `Key of key | `Dir of key ] -> 'a -> 'a Lwt.t) ->
    'a Lwt.t

  val keys: context -> key -> key list Lwt.t

  val fold_keys:
    context -> key -> init:'a -> f:(key -> 'a -> 'a Lwt.t) -> 'a Lwt.t

  val project: context -> root_context

  val absolute_key: context -> key -> key

  (*
  val consume_gas: context -> Gas_limit_repr.cost -> context tzresult

  val check_enough_gas: context -> Gas_limit_repr.cost -> unit tzresult

  val description: context Storage_description.t
*)
end


