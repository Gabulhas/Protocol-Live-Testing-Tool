(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs, <contact@nomadic-labs.com>               *)
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

(** FIXME: https://gitlab.com/tezos/tezos/-/issues/3517

    If the layer1 node reboots, the rpc stream breaks.
*)
let chain_events cctxt =
  let open Lwt_result_syntax in
  let* heads, _ = Tezos_shell_services.Monitor_services.heads cctxt `Main in
  return heads

let handle_event (hash, (block_header : Tezos_base.Block_header.t)) =
  let open Lwt_result_syntax in
  let level = block_header.shell.level in
  let*! () = Event.(emit layer1_node_new_head (hash, level)) in
  return_true

let iter_events cctxt handle =
  let open Lwt_result_syntax in
  let* stream = chain_events cctxt in
  let rec go () =
    Lwt.bind (Lwt_stream.get stream) @@ fun tok ->
    match tok with
    | None -> return_unit
    | Some element ->
        let* () = handle element in
        go ()
  in
  go ()
