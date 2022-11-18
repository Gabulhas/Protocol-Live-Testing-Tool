(*****************************************************************************)

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

(*type t = IncrA | IncrB | Transfer of Int32.t*)

type t = Transaction_to_implicit of {destination: Signature.Public_key_hash.t; amount: Tez_repr.tez} 


let encoding =
  let open Data_encoding in
  let caseTransaction_to_implicit =
    let dest = function Transaction_to_implicit {destination; amount} -> Some (destination, amount) in
    let const (destination, amount) = Transaction_to_implicit {destination; amount} in
    case ~title:"Transaction_to_implicit" (Tag 0) (obj2 (req "Destination" Signature.Public_key_hash.encoding) (req "Amount" Tez_repr.encoding)) dest const
  in
  union [caseTransaction_to_implicit]

let compare x y =
    match x, y with
    | Transaction_to_implicit {destination= a_d; amount= a_a}, Transaction_to_implicit {destination= b_d; amount= b_a} -> (
        let open Signature.Public_key_hash in  
        if a_d = b_d then 
            Tez_repr.compare a_a b_a 
        else 
            compare a_d b_d
    )
