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

let max_block_length = 100

let max_operation_data_length = 100

let validation_passes = Updater.[{max_size = 1000; max_op = None}]

let acceptable_pass _op = Some 0

type block_header_data = Header.t

type block_header = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}

let block_header_data_encoding = Header.encoding

type block_header_metadata = State.t

let block_header_metadata_encoding = State.encoding

type operation_data = Proto_operation.t

let operation_data_encoding = Proto_operation.encoding

type operation_receipt = Receipt.t

let operation_receipt_encoding = Receipt.encoding

let operation_data_and_receipt_encoding =
  (* we could merge data and receipt encoding for a lighter json *)
  Data_encoding.(
    obj2 (req "data" Proto_operation.encoding) (req "receipt" Receipt.encoding))

type operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

type validation_state = {context : Context.t; fitness : Fitness.t}

type application_state = validation_state

type mode =
  | Application of block_header
  | Partial_validation of block_header
  | Construction of {
      predecessor_hash : Block_hash.t;
      timestamp : Time.t;
      block_header_data : block_header_data;
    }
  | Partial_construction of {
      predecessor_hash : Block_hash.t;
      timestamp : Time.t;
    }

let mode_str = function
  | Application _ -> "application"
  | Partial_validation _ -> "partial_validation"
  | Construction _ -> "construction"
  | Partial_construction _ -> "partial_construction"

let validation_or_application_str = function
  | `Validation -> "validation"
  | `Application -> "application"

