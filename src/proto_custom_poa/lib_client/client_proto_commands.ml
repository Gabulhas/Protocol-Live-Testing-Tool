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

open Protocol
open Protocol_client_context
module Receipt = Receipt_repr

let validator_order_at_level (cctxt : Protocol_client_context.full) level =
  Services.PoAServices.Commands.validator_order_at_level
    cctxt
    (cctxt#chain, cctxt#block)
    level
  >>=? fun order -> return order

let skips_at_timestamp (cctxt : Protocol_client_context.full) previous_timestamp
    current_timestamp =
  Services.PoAServices.Commands.skips_at_timestamp
    cctxt
    (cctxt#chain, cctxt#block)
    previous_timestamp
    current_timestamp
  >>=? fun skips -> return skips

let first_block_validator (cctxt : Protocol_client_context.full) level =
  Services.PoAServices.Commands.first_block_validator
    cctxt
    (cctxt#chain, cctxt#block)
    level
  >>=? fun validator -> return validator

let expected_validator (cctxt : Protocol_client_context.full) level
    previous_timestamp current_timestamp =
  Services.PoAServices.Commands.expected_validator
    cctxt
    (cctxt#chain, cctxt#block)
    level
    previous_timestamp
    current_timestamp
  >>=? fun validator -> return validator

let validator_set (cctxt : Protocol_client_context.full) =
  Services.PoAServices.Commands.validator_set cctxt (cctxt#chain, cctxt#block)
  >>=? fun set -> return set

let get_balance (cctxt : Protocol_client_context.full) account =
  Services.AccountServices.Commands.get_balance
    cctxt
    (cctxt#chain, cctxt#block)
    account
  >>=? fun cnt -> return cnt

let get_current_counter (cctxt : Protocol_client_context.full) account =
  Services.AccountServices.Commands.get_counter
    cctxt
    (cctxt#chain, cctxt#block)
    account
  >>=? fun cnt -> return cnt

let fetch_all_constants (cctxt : Protocol_client_context.full) () =
  Services.ConstantServices.Commands.fetch_all_constants
    cctxt
    (cctxt#chain, cctxt#block)
    ()
  >>=? fun constants -> return constants

let get_new_counter (cctxt : Protocol_client_context.full) account =
  Services.AccountServices.Commands.get_counter
    cctxt
    (cctxt#chain, cctxt#block)
    account
  >>=? fun cnt -> return (Z.succ cnt)

let is_revealed (cctxt : Protocol_client_context.full) account =
  Services.AccountServices.Commands.revealed
    cctxt
    (cctxt#chain, cctxt#block)
    account
  >>=? fun is_revealed -> return is_revealed

let get_operation_header (cctxt : Protocol_client_context.full) =
  Alpha_block_services.hash cctxt () >|=? fun (block_hash : Block_hash.t) ->
  Operation.{branch = block_hash}

let inject_op (cctxt : Protocol_client_context.full)
    (op : Operation_repr.operation) =
  Alpha_block_services.Helpers.Preapply.operations cctxt [op] >>=? function
  | [(_op_data, op_receipt)] ->
      let json_value =
        Data_encoding.Json.construct
          Apply_results.operation_result_encoding
          op_receipt
      in
      let receipt_str = Data_encoding.Json.to_string json_value in
      cctxt#message "Operation receipt: %s" receipt_str >>= fun () ->
      let mbytes =
        Data_encoding.Binary.to_bytes_exn Operation_repr.encoding op
      in
      Shell_services.Injection.operation ~async:false cctxt mbytes
      >>=? fun op_hash ->
      let injected = Operation_hash.to_short_b58check op_hash in
      cctxt#message "Injected: %s" injected >>= fun () -> return_unit
  | _ -> assert false

let build_manager_operation_protocol_data cctxt (src_pk : Signature.public_key)
    src_sk operation_content counter_opt =
  let open Operation_repr in
  let src_hash = Signature.Public_key.hash src_pk in

  (match counter_opt with
  | None -> get_current_counter cctxt src_hash
  | Some c -> Lwt.return_ok c)
  >>=? fun current_counter ->
  get_operation_header cctxt >>=? fun shell_header ->
  let fee = Tez_repr.zero in
  let protocol_data_content =
    Management
      {
        source = src_hash;
        fee;
        counter = Z.succ current_counter;
        content = operation_content;
      }
  in
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      unsigned_operation_encoding
      (shell_header, protocol_data_content)
  in

  let watermark = Signature.Generic_operation in
  Client_keys.sign cctxt ~watermark src_sk bytes >>=? fun signature ->
  return
    {
      shell = shell_header;
      protocol_data =
        {content = protocol_data_content; signature = Some signature};
    }
