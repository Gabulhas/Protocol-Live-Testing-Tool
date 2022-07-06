(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2021 Nomadic Labs, <contact@nomadic-labs.com>               *)
(* Copyright (c) 2022 Trili Tech, <contact@trili.tech>                       *)
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

(** Purposes for operators, indicating the kind of operations that they sign. *)
type purpose = Publish | Add_messages | Cement | Refute

module Operator_purpose_map : Map.S with type key = purpose

type operators = Signature.Public_key_hash.t Operator_purpose_map.t

type t = {
  data_dir : string;
  sc_rollup_address : Protocol.Alpha_context.Sc_rollup.t;
  sc_rollup_node_operators : operators;
  rpc_addr : string;
  rpc_port : int;
  fee_parameter : Injection.fee_parameter;
  loser_mode : Loser_mode.t;
}

(** [make_purpose_map ~default purposes] constructs a purpose map from a list of
    bindings [purposes], with a potential [default] value. *)
val make_purpose_map :
  default:'a option -> (purpose * 'a) trace -> 'a Operator_purpose_map.t

(** Parses a purpose.
    @raise Invalid_argument if not a valid purpose *)
val purpose_of_string : string -> purpose

(** [default_data_dir] is the default value for [data_dir]. *)
val default_data_dir : string

(** [default_storage_dir] returns the default value of the storage dir
    given a [data_dir]. *)
val default_storage_dir : string -> string

(** [default_rpc_addr] is the default value for [rpc_addr]. *)
val default_rpc_addr : string

(** [default_rpc_port] is the default value for [rpc_port]. *)
val default_rpc_port : int

(** [default_fee_parameter] is the default value for [fee_parameter] *)
val default_fee_parameter : Injection.fee_parameter

(** [filename configuration] returns the [configuration] filename. *)
val filename : t -> string

(** [save configuration] overwrites [configuration] file. *)
val save : t -> unit tzresult Lwt.t

(** [load ~data_dir] loads a configuration stored in [data_dir]. *)
val load : data_dir:string -> t tzresult Lwt.t
