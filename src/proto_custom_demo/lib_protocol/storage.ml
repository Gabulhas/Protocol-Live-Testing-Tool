(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

open Storage_functors
open Storage_sigs

module Encoding = struct
  module UInt16 : VALUE with type t = int = struct
    type t = int

    let encoding = Data_encoding.uint16
  end

  module Int32 : VALUE with type t = Int32.t = struct
    type t = Int32.t

    let encoding = Data_encoding.int32
  end

  module Int64 : VALUE with type t = Int64.t = struct
    type t = Int64.t

    let encoding = Data_encoding.int64
  end

  module Z : VALUE with type t = Z.t = struct
    type t = Z.t

    let encoding = Data_encoding.z
  end
end

module Int31_index : INDEX with type t = int = struct
  type t = int

  let path_length = 1

  let to_path c l = string_of_int c :: l

  let of_path = function [] | _ :: _ :: _ -> None | [c] -> int_of_string_opt c

  type 'a ipath = 'a * t

  let args =
    Storage_description.One
      {
        rpc_arg = RPC_arg.int;
        encoding = Data_encoding.int31;
        compare = Compare.Int.compare;
      }
end

module Make_index (H : Storage_description.INDEX) :
  INDEX with type t = H.t and type 'a ipath = 'a * H.t = struct
  include H

  type 'a ipath = 'a * t

  let args = Storage_description.One {rpc_arg; encoding; compare}
end

module type Simple_single_data_storage = sig
  type value

  val get : Raw_context.t -> value tzresult Lwt.t

  val update : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t

  val init : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t
end

module Contract = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["contracts"]
      end)

  module Global_counter : Simple_single_data_storage with type value = Z.t =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["global_counter"]
      end)
      (Encoding.Z)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Account_storage.Index))

  let fold = Indexed_context.fold_keys

  let list = Indexed_context.keys

  module Spendable_balance =
    Indexed_context.Make_map
      (struct
        let name = ["balance"]
      end)
      (Tez_repr)

  module Manager =
    Indexed_context.Make_map
      (struct
        let name = ["manager"]
      end)
      (Manager_repr)

  module Counter =
    Indexed_context.Make_map
      (struct
        let name = ["counter"]
      end)
      (Encoding.Z)

end

module type NEXT = sig
  type id

  val init : Raw_context.t -> Raw_context.t tzresult Lwt.t

  val incr : Raw_context.t -> (Raw_context.t * id) tzresult Lwt.t
end

module Public_key_hash = struct
  open Signature
  include Signature.Public_key_hash
  module Path_Ed25519 = Path_encoding.Make_hex (Ed25519.Public_key_hash)
  module Path_Secp256k1 = Path_encoding.Make_hex (Secp256k1.Public_key_hash)
  module Path_P256 = Path_encoding.Make_hex (P256.Public_key_hash)

  let to_path (key : public_key_hash) l =
    match key with
    | Ed25519 h -> "ed25519" :: Path_Ed25519.to_path h l
    | Secp256k1 h -> "secp256k1" :: Path_Secp256k1.to_path h l
    | P256 h -> "p256" :: Path_P256.to_path h l

  let of_path : _ -> public_key_hash option = function
    | "ed25519" :: rest -> (
        match Path_Ed25519.of_path rest with
        | Some pkh -> Some (Ed25519 pkh)
        | None -> None)
    | "secp256k1" :: rest -> (
        match Path_Secp256k1.of_path rest with
        | Some pkh -> Some (Secp256k1 pkh)
        | None -> None)
    | "p256" :: rest -> (
        match Path_P256.of_path rest with
        | Some pkh -> Some (P256 pkh)
        | None -> None)
    | _ -> None

  let path_length =
    let l1 = Path_Ed25519.path_length
    and l2 = Path_Secp256k1.path_length
    and l3 = Path_P256.path_length in
    assert (Compare.Int.(l1 = l2 && l2 = l3)) ;
    l1 + 1
end

module Public_key_hash_index = Make_index (Public_key_hash)

module Protocol_hash_with_path_encoding = struct
  include Protocol_hash
  include Path_encoding.Make_hex (Protocol_hash)
end

