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
  temporary_big_map: Z.t ;
  internal_nonce: int ;
  internal_nonces_used: Int_set.t ;
}

type context = t
type root_context = t

let current_timestamp ctxt = ctxt.timestamp
let current_fitness ctxt = ctxt.fitness
let recover ctxt = ctxt.context

type error += Too_many_internal_operations (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"too_many_internal_operations"
    ~title: "Too many internal operations"
    ~description:
      "A transaction exceeded the hard limit \
       of internal operations it can emit"
    empty
    (function Too_many_internal_operations -> Some () | _ -> None)
    (fun () -> Too_many_internal_operations)

let fresh_internal_nonce ctxt =
  if Compare.Int.(ctxt.internal_nonce >= 65_535) then
    error Too_many_internal_operations
  else
    ok ({ ctxt with internal_nonce = ctxt.internal_nonce + 1 }, ctxt.internal_nonce)
let reset_internal_nonce ctxt =
  { ctxt with internal_nonces_used = Int_set.empty ; internal_nonce = 0 }
let record_internal_nonce ctxt k =
  { ctxt with internal_nonces_used = Int_set.add k ctxt.internal_nonces_used }
let internal_nonce_already_recorded ctxt k =
  Int_set.mem k ctxt.internal_nonces_used

let set_current_fitness ctxt fitness = { ctxt with fitness }

let add_fees ctxt fees =
  Lwt.return Tez_repr.(ctxt.fees +? fees) >>=? fun fees ->
  return { ctxt with fees}

let add_rewards ctxt rewards =
  Lwt.return Tez_repr.(ctxt.rewards +? rewards) >>=? fun rewards ->
  return { ctxt with rewards}

let get_rewards ctxt = ctxt.rewards
let get_fees ctxt = ctxt.fees

type error += Undefined_operation_nonce (* `Permanent *)

let () =
  let open Data_encoding in
  register_error_kind
    `Permanent
    ~id:"undefined_operation_nonce"
    ~title: "Ill timed access to the origination nonce"
    ~description:
      "An origination was attemped out of the scope of a manager operation"
    empty
    (function Undefined_operation_nonce -> Some () | _ -> None)
    (fun () -> Undefined_operation_nonce)



(* Initialization *********************************************************)

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]
let version_value = "babylon_005"

let version = "v1"
let first_level_key = [ version ; "first_level" ]
let constants_key = [ version ; "constants" ]
let protocol_param_key = [ "protocol_parameters" ]

type error += Failed_to_decode_parameter of Data_encoding.json * string

let get_proto_param ctxt =
  Context.get ctxt protocol_param_key >>= function
  | None ->
      failwith "Missing protocol parameters."
  | Some bytes ->
      match Data_encoding.Binary.of_bytes Data_encoding.json bytes with
      | None -> fail (Failed_to_parse_parameter bytes)
      | Some json -> begin
          Context.del ctxt protocol_param_key >>= fun ctxt ->
          match Data_encoding.Json.destruct Parameters_repr.encoding json with
          | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
              Format.kasprintf
                failwith "Invalid protocol_parameters: %a %a"
                (fun ppf -> Data_encoding.Json.print_error ppf) exn
                Data_encoding.Json.pp json
          | param -> return (param, ctxt)
        end

let set_constants ctxt constants =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Constants_repr.parametric_encoding constants in
  Context.set ctxt constants_key bytes

let check_inited ctxt =
  Context.get ctxt version_key >>= function
  | None ->
      failwith "Internal error: un-initialized context."
  | Some bytes ->
      let s = MBytes.to_string bytes in
      if Compare.String.(s = version_value) then
        return_unit
      else
        storage_error (Incompatible_protocol_version s)

let prepare ~level ~predecessor_timestamp ~timestamp ~fitness ctxt =
  Lwt.return (Raw_level_repr.of_int32 level) >>=? fun level ->
  Lwt.return (Fitness_repr.to_int64 fitness) >>=? fun fitness ->
  check_inited ctxt >>=? fun () ->
  get_constants ctxt >>=? fun constants ->
  get_first_level ctxt >>=? fun first_level ->
  let level =
    Level_repr.from_raw
      ~first_level
      ~blocks_per_cycle:constants.Constants_repr.blocks_per_cycle
      ~blocks_per_voting_period:constants.Constants_repr.blocks_per_voting_period
      ~blocks_per_commitment:constants.Constants_repr.blocks_per_commitment
      level in
  return {
    context = ctxt ; constants ; level ;
    predecessor_timestamp ;
    timestamp ; fitness ; first_level ;
    allowed_endorsements = Signature.Public_key_hash.Map.empty ;
    included_endorsements = 0 ;
    fees = Tez_repr.zero ;
    rewards = Tez_repr.zero ;
    deposits = Signature.Public_key_hash.Map.empty ;
    operation_gas = Unaccounted ;
    internal_gas = Gas_limit_repr.internal_gas_zero ;
    storage_space_to_pay = None ;
    allocated_contracts = None ;
    block_gas = constants.Constants_repr.hard_gas_limit_per_block ;
    origination_nonce = None ;
    temporary_big_map = Z.sub Z.zero Z.one ;
    internal_nonce = 0 ;
    internal_nonces_used = Int_set.empty ;
  }

type previous_protocol =
  | Genesis of Parameters_repr.t
  | Athens_004

let check_and_update_protocol_version ctxt =
  begin
    Context.get ctxt version_key >>= function
    | None ->
        failwith "Internal error: un-initialized context in check_first_block."
    | Some bytes ->
        let s = MBytes.to_string bytes in
        if Compare.String.(s = version_value) then
          failwith "Internal error: previously initialized context."
        else if Compare.String.(s = "genesis") then
          get_proto_param ctxt >>=? fun (param, ctxt) ->
          return (Genesis param, ctxt)
        else if Compare.String.(s = "athens_004") then
          return (Athens_004, ctxt)
        else
          storage_error (Incompatible_protocol_version s)
  end >>=? fun (previous_proto, ctxt) ->
  Context.set ctxt version_key
    (MBytes.of_string version_value) >>= fun ctxt ->
  return (previous_proto, ctxt)

let prepare_first_block ~level ~timestamp ~fitness ctxt =
  check_and_update_protocol_version ctxt >>=? fun (previous_proto, ctxt) ->
  begin
    match previous_proto with
    | Genesis param ->
        Lwt.return (Raw_level_repr.of_int32 level) >>=? fun first_level ->
        set_first_level ctxt first_level >>=? fun ctxt ->
        set_constants ctxt param.constants >>= fun ctxt ->
        return ctxt
    | Athens_004 ->
        get_004_constants ctxt >>=? fun c ->
        let constants = Constants_repr.{
            preserved_cycles = c.preserved_cycles ;
            blocks_per_cycle = c.blocks_per_cycle ;
            blocks_per_commitment = c.blocks_per_commitment ;
            blocks_per_roll_snapshot = c.blocks_per_roll_snapshot ;
            blocks_per_voting_period = c.blocks_per_voting_period ;
            time_between_blocks =
              List.map Period_repr.of_seconds_exn [ 60L ; 40L ] ;
            endorsers_per_block = c.endorsers_per_block ;
            hard_gas_limit_per_operation = c.hard_gas_limit_per_operation ;
            hard_gas_limit_per_block = c.hard_gas_limit_per_block ;
            proof_of_work_threshold = c.proof_of_work_threshold ;
            tokens_per_roll = c.tokens_per_roll ;
            michelson_maximum_type_size = c.michelson_maximum_type_size;
            seed_nonce_revelation_tip = c.seed_nonce_revelation_tip ;
            origination_size = c.origination_size ;
            block_security_deposit = c.block_security_deposit ;
            endorsement_security_deposit = c.endorsement_security_deposit ;
            block_reward = c.block_reward ;
            endorsement_reward = c.endorsement_reward ;
            cost_per_byte = c.cost_per_byte ;
            hard_storage_limit_per_operation = c.hard_storage_limit_per_operation ;
            test_chain_duration = c.test_chain_duration ;
            quorum_min = 20_00l ; (* quorum is in centile of a percentage *)
            quorum_max = 70_00l ;
            min_proposal_quorum = 5_00l ;
            initial_endorsers = 24 ;
            delay_per_missing_endorsement = Period_repr.of_seconds_exn 8L ;
          } in
        set_constants ctxt constants >>= fun ctxt ->
        return ctxt
  end >>=? fun ctxt ->
  prepare ctxt ~level ~predecessor_timestamp:timestamp ~timestamp ~fitness >>=? fun ctxt ->
  return (previous_proto, ctxt)

let activate ({ context = c ; _ } as s) h =
  Updater.activate c h >>= fun c -> Lwt.return { s with context = c }

let fork_test_chain ({ context = c ; _ } as s) protocol expiration =
  Updater.fork_test_chain c ~protocol ~expiration >>= fun c ->
  Lwt.return { s with context = c }

(* Generic context ********************************************************)

type key = string list

type value = MBytes.t

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

  val consume_gas: context -> Gas_limit_repr.cost -> context tzresult

  val check_enough_gas: context -> Gas_limit_repr.cost -> unit tzresult

  val description: context Storage_description.t

end

let mem ctxt k = Context.mem ctxt.context k
let dir_mem ctxt k = Context.dir_mem ctxt.context k

let get ctxt k =
  Context.find ctxt.context k >>= function
  | None -> storage_error (Missing_key (k, `Get))
  | Some v -> return v

let get_option ctxt k =
  Context.get ctxt.context k

(* Verify that the k is present before modifying *)
let set ctxt k v =
  Context.mem ctxt.context k >>= function
  | false -> storage_error (Missing_key (k, `Set))
  | true ->
      Context.set ctxt.context k v >>= fun context ->
      return { ctxt with context }

(* Verify that the k is not present before inserting *)
let init ctxt k v =
  Context.mem ctxt.context k >>= function
  | true -> storage_error (Existing_key k)
  | false ->
      Context.set ctxt.context k v >>= fun context ->
      return { ctxt with context }

(* Does not verify that the key is present or not *)
let init_set ctxt k v =
  Context.set ctxt.context k v >>= fun context ->
  Lwt.return { ctxt with context }

(* Verify that the key is present before deleting *)
let delete ctxt k =
  Context.mem ctxt.context k >>= function
  | false -> storage_error (Missing_key (k, `Del))
  | true ->
      Context.del ctxt.context k >>= fun context ->
      return { ctxt with context }

(* Do not verify before deleting *)
let remove ctxt k =
  Context.del ctxt.context k >>= fun context ->
  Lwt.return { ctxt with context }

let set_option ctxt k = function
  | None -> remove ctxt k
  | Some v -> init_set ctxt k v

let remove_rec ctxt k =
  Context.remove_rec ctxt.context k >>= fun context ->
  Lwt.return { ctxt with context }

let copy ctxt ~from ~to_ =
  Context.copy ctxt.context ~from ~to_ >>= function
  | None -> storage_error (Missing_key (from, `Copy))
  | Some context ->
      return { ctxt with context }

let fold ctxt k ~init ~f =
  Context.fold ctxt.context k ~init ~f

let keys ctxt k =
  Context.keys ctxt.context k

let fold_keys ctxt k ~init ~f =
  Context.fold_keys ctxt.context k ~init ~f

let project x = x

let absolute_key _ k = k

let description = Storage_description.create ()

let fresh_temporary_big_map ctxt =
  { ctxt with temporary_big_map = Z.sub ctxt.temporary_big_map Z.one },
  ctxt.temporary_big_map

let reset_temporary_big_map ctxt =
  { ctxt with temporary_big_map = Z.sub Z.zero Z.one }

let temporary_big_maps ctxt f acc =
  let rec iter acc id =
    if Z.equal id ctxt.temporary_big_map then
      Lwt.return acc
    else
      f acc id >>= fun acc ->
      iter acc (Z.sub id Z.one) in
  iter acc (Z.sub Z.zero Z.one)

