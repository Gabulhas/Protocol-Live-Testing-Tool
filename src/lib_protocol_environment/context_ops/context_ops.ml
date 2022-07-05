(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

module Environment_context = Tezos_protocol_environment.Context
module Memory_context = Tezos_protocol_environment.Memory_context

let err_implementation_mismatch =
  Tezos_protocol_environment.err_implementation_mismatch
(* Backend-agnostic operations on the context *)

let mem (context : Environment_context.t) key =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.mem ctxt key
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.mem ctxt key
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_protocol (context : Environment_context.t) proto_hash =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_protocol ctxt proto_hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add_protocol ctxt proto_hash in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let get_protocol (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} -> Context.get_protocol ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.get_protocol ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_predecessor_block_metadata_hash (context : Environment_context.t) hash =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_predecessor_block_metadata_hash ctxt hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.add_predecessor_block_metadata_hash
          ctxt
          hash
      in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_predecessor_ops_metadata_hash (context : Environment_context.t) hash =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_predecessor_ops_metadata_hash ctxt hash in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt =
        Tezos_context_memory.Context.add_predecessor_ops_metadata_hash ctxt hash
      in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let hash ~time ?message (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.hash ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.hash ~time ?message ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let get_test_chain (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.get_test_chain ctxt
  | Context {kind = Memory_context.Context; _} ->
      Lwt.return Test_chain_status.Not_running
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let add_test_chain (context : Environment_context.t) status =
  let open Lwt_syntax in
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      let+ ctxt = Context.add_test_chain ctxt status in
      Shell_context.wrap_disk_context ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      let+ ctxt = Tezos_context_memory.Context.add_test_chain ctxt status in
      Memory_context.wrap_memory_context ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let commit ~time ?message (context : Environment_context.t) =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.commit ~time ?message ctxt
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.commit ~time ?message ctxt
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let commit_test_chain_genesis (context : Environment_context.t) block_header =
  match context with
  | Context {kind = Shell_context.Context; ctxt; _} ->
      Context.commit_test_chain_genesis ctxt block_header
  | Context {kind = Memory_context.Context; ctxt; _} ->
      Tezos_context_memory.Context.commit_test_chain_genesis ctxt block_header
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name

let compute_testchain_genesis (context : Environment_context.t) block_hash =
  match context with
  | Context {kind = Shell_context.Context; _} ->
      Context.compute_testchain_genesis block_hash
  | Context {kind = Memory_context.Context; _} ->
      Tezos_context_memory.Context.compute_testchain_genesis block_hash
  | Context t ->
      err_implementation_mismatch ~expected:"shell or memory" ~got:t.impl_name