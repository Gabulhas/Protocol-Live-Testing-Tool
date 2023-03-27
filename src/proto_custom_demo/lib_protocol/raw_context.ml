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

module Int_set = Set.Make (Compare.Int)

type t = {
  context : Context.t;
  constants : Constants_repr.parametric;
  timestamp : Time.t;
  level : Int32.t;
  first_level : Int32.t;
}

let[@inline] context ctxt = ctxt.context

let[@inline] constants ctxt = ctxt.constants

let[@inline] level ctxt = ctxt.level
let[@inline] first_level ctxt = ctxt.level

let[@inline] timestamp ctxt = ctxt.timestamp

let[@inline] update_context ctxt context = {ctxt with context}

let[@inline] update_constants ctxt constants = {ctxt with constants}

type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

type error += Storage_error of storage_error

let storage_error err = error (Storage_error err)

type error += Block_quota_exceeded (* `Temporary *)

type error += Operation_quota_exceeded (* `Temporary *)

(* This key should always be populated for every version of the
   protocol.  It's absence meaning that the context is empty. *)
let version_key = ["version"]

(* This value is set by the snapshot_alpha.sh script, don't change it. *)
let version_value = "custom_protocol"

let version = "v1"

let first_level_key = [version; "first_level"]

let constants_key = [version; "constants"]

let protocol_param_key = ["protocol_parameters"]

let get_first_level ctxt =
  Logging.log Notice "Getting first level" ;
  Context.find ctxt first_level_key >|= function
  | None -> storage_error (Missing_key (first_level_key, Get))
  | Some bytes -> (
      match Data_encoding.Binary.of_bytes_opt Raw_level_repr.encoding bytes with
      | None -> storage_error (Corrupted_data first_level_key)
      | Some level -> ok level)

let set_first_level ctxt level =
  let bytes = Data_encoding.Binary.to_bytes_exn Raw_level_repr.encoding level in
  Context.add ctxt first_level_key bytes >|= ok

type error += Failed_to_parse_parameter of bytes

type error += Failed_to_decode_parameter of Data_encoding.json * string

let () =
  register_error_kind
    `Temporary
    ~id:"context.failed_to_parse_parameter"
    ~title:"Failed to parse parameter"
    ~description:"The protocol parameters are not valid JSON."
    ~pp:(fun ppf bytes ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot parse the protocol parameter:@ %s@]"
        (Bytes.to_string bytes))
    Data_encoding.(obj1 (req "contents" bytes))
    (function Failed_to_parse_parameter data -> Some data | _ -> None)
    (fun data -> Failed_to_parse_parameter data) ;
  register_error_kind
    `Temporary
    ~id:"context.failed_to_decode_parameter"
    ~title:"Failed to decode parameter"
    ~description:"Unexpected JSON object."
    ~pp:(fun ppf (json, msg) ->
      Format.fprintf
        ppf
        "@[<v 2>Cannot decode the protocol parameter:@ %s@ %a@]"
        msg
        Data_encoding.Json.pp
        json)
    Data_encoding.(obj2 (req "contents" json) (req "error" string))
    (function
      | Failed_to_decode_parameter (json, msg) -> Some (json, msg) | _ -> None)
    (fun (json, msg) -> Failed_to_decode_parameter (json, msg))

let get_proto_param ctxt =
  Context.find ctxt protocol_param_key >>= function
  | None -> failwith "Missing protocol parameters."
  | Some bytes -> (
      match Data_encoding.Binary.of_bytes_opt Data_encoding.json bytes with
      | None -> fail (Failed_to_parse_parameter bytes)
      | Some json -> (
          Context.remove ctxt protocol_param_key >|= fun ctxt ->
          match Data_encoding.Json.destruct Parameters_repr.encoding json with
          | exception (Data_encoding.Json.Cannot_destruct _ as exn) ->
              Format.kasprintf
                failwith
                "Invalid protocol_parameters: %a %a"
                (fun ppf -> Data_encoding.Json.print_error ppf)
                exn
                Data_encoding.Json.pp
                json
          | param -> ok (param, ctxt)))

let add_constants ctxt constants =
  let bytes =
    Data_encoding.Binary.to_bytes_exn
      Constants_repr.parametric_encoding
      constants
  in
  Context.add ctxt constants_key bytes

let get_constants ctxt =
  Context.find ctxt constants_key >|= function
  | None -> failwith "Internal error: cannot read constants in context."
  | Some bytes -> (
      match
        Data_encoding.Binary.of_bytes_opt
          Constants_repr.parametric_encoding
          bytes
      with
      | None -> failwith "Internal error: cannot parse constants in context."
      | Some constants -> ok constants)

let _patch_constants ctxt f =
  let constants = f (constants ctxt) in
  add_constants (context ctxt) constants >|= fun context ->
  let ctxt = update_context ctxt context in
  update_constants ctxt constants

let _check_inited ctxt =
  Context.find ctxt version_key >|= function
  | None -> failwith "Internal error: un-initialized context."
  | Some bytes ->
      let s = Bytes.to_string bytes in
      if Compare.String.(s = version_value) then Ok ()
      else storage_error (Incompatible_protocol_version s)

let prepare ctxt ~level ~timestamp =
  Logging.log
    Notice
    "prepare: Level %s | Timestamp %s"
    (Int32.to_string level)
    (Time.to_notation timestamp) ;
  get_first_level ctxt >>=? fun first_level ->
  get_constants ctxt >|=? fun constants ->
  {context = ctxt; constants; timestamp; level; first_level}

type previous_protocol = Genesis of Parameters_repr.t

let check_and_update_protocol_version ctxt =
  Logging.log Notice "check_and_update_protocol_version" ;
  (Context.find ctxt version_key >>= function
   | None ->
       failwith "Internal error: un-initialized context in check_first_block."
   | Some bytes ->
       let s = Bytes.to_string bytes in
       if Compare.String.(s = version_value) then
         failwith "Internal error: previously initialized context."
       else if Compare.String.(s = "genesis") then
         get_proto_param ctxt >|=? fun (param, ctxt) -> (Genesis param, ctxt)
       else Lwt.return @@ storage_error (Incompatible_protocol_version s))
  >>=? fun (previous_proto, ctxt) ->
  Context.add ctxt version_key (Bytes.of_string version_value) >|= fun ctxt ->
  ok (previous_proto, ctxt)

let prepare_first_block ctxt ~level ~timestamp =
  Logging.log
    Notice
    "prepare_first_block: %s %s"
    (Int32.to_string level)
    (Time.to_notation timestamp) ;
  check_and_update_protocol_version ctxt >>=? fun (previous_proto, ctxt) ->
  (match previous_proto with
  | Genesis param ->
      Raw_level_repr.of_int32 level >>?= fun first_level ->
      set_first_level ctxt first_level >>=? fun ctxt ->
      add_constants ctxt param.constants >|= ok)
  >>=? fun ctxt ->
  prepare ctxt ~level ~timestamp >|=? fun ctxt -> (previous_proto, ctxt)

let activate ctxt h =
  Logging.log Notice "Activating" ;
  Updater.activate (context ctxt) h >|= update_context ctxt

(* Generic context ********************************************************)

type root = t

type key = string list

type value = bytes

type tree = Context.tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

let mem ctxt k = Context.mem (context ctxt) k

let mem_tree ctxt k = Context.mem_tree (context ctxt) k

let get ctxt k =
  Context.find (context ctxt) k >|= function
  | None -> storage_error (Missing_key (k, Get))
  | Some v -> ok v

let get_tree ctxt k =
  Context.find_tree (context ctxt) k >|= function
  | None -> storage_error (Missing_key (k, Get))
  | Some v -> ok v

let find ctxt k = Context.find (context ctxt) k

let find_tree ctxt k = Context.find_tree (context ctxt) k

let add ctxt k v = Context.add (context ctxt) k v >|= update_context ctxt

let add_tree ctxt k v =
  Context.add_tree (context ctxt) k v >|= update_context ctxt

let init ctxt k v =
  Context.mem (context ctxt) k >>= function
  | true -> Lwt.return @@ storage_error (Existing_key k)
  | _ ->
      Context.add (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let init_tree ctxt k v : _ tzresult Lwt.t =
  Context.mem_tree (context ctxt) k >>= function
  | true -> Lwt.return @@ storage_error (Existing_key k)
  | _ ->
      Context.add_tree (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let update ctxt k v =
  Context.mem (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
      Context.add (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

let update_tree ctxt k v =
  Context.mem_tree (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
  | _ ->
      Context.add_tree (context ctxt) k v >|= fun context ->
      ok (update_context ctxt context)

(* Verify that the key is present before deleting *)
let remove_existing ctxt k =
  Context.mem (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
      Context.remove (context ctxt) k >|= fun context ->
      ok (update_context ctxt context)

(* Verify that the key is present before deleting *)
let remove_existing_tree ctxt k =
  Context.mem_tree (context ctxt) k >>= function
  | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
  | _ ->
      Context.remove (context ctxt) k >|= fun context ->
      ok (update_context ctxt context)

(* Do not verify before deleting *)
let remove ctxt k = Context.remove (context ctxt) k >|= update_context ctxt

let add_or_remove ctxt k = function
  | None -> remove ctxt k
  | Some v -> add ctxt k v

let add_or_remove_tree ctxt k = function
  | None -> remove ctxt k
  | Some v -> add_tree ctxt k v

let list ctxt ?offset ?length k = Context.list (context ctxt) ?offset ?length k

let fold ?depth ctxt k ~order ~init ~f =
  Context.fold ?depth (context ctxt) k ~order ~init ~f

let description = Storage_description.create ()

let config ctxt = Context.config (context ctxt)

module Proof = Context.Proof

let length ctxt key = Context.length (context ctxt) key

module Tree :
  Raw_context_intf.TREE
    with type t := t
     and type key := key
     and type value := value
     and type tree := tree = struct
  include Context.Tree

  let empty ctxt = Context.Tree.empty (context ctxt)

  let get t k =
    find t k >|= function
    | None -> storage_error (Missing_key (k, Get))
    | Some v -> ok v

  let get_tree t k =
    find_tree t k >|= function
    | None -> storage_error (Missing_key (k, Get))
    | Some v -> ok v

  let init t k v =
    mem t k >>= function
    | true -> Lwt.return @@ storage_error (Existing_key k)
    | _ -> add t k v >|= ok

  let init_tree t k v =
    mem_tree t k >>= function
    | true -> Lwt.return @@ storage_error (Existing_key k)
    | _ -> add_tree t k v >|= ok

  let update t k v =
    mem t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ -> add t k v >|= ok

  let update_tree t k v =
    mem_tree t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Set))
    | _ -> add_tree t k v >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing t k =
    mem t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ -> remove t k >|= ok

  (* Verify that the key is present before deleting *)
  let remove_existing_tree t k =
    mem_tree t k >>= function
    | false -> Lwt.return @@ storage_error (Missing_key (k, Del))
    | _ -> remove t k >|= ok

  let add_or_remove t k = function None -> remove t k | Some v -> add t k v

  let add_or_remove_tree t k = function
    | None -> remove t k
    | Some v -> add_tree t k v
end

let verify_tree_proof proof f = Context.verify_tree_proof proof f

let verify_stream_proof proof f = Context.verify_stream_proof proof f

let equal_config = Context.equal_config

let project x = x

let absolute_key _ k = k

(****** GAS STUFF ***********)

let consume_gas ctxt _cost = ok ctxt

let check_enough_gas ctxt cost =
  consume_gas ctxt cost >>? fun _ -> Result.return_unit

module Cache = struct
  type key = Context.Cache.key

  type value = Context.Cache.value = ..

  let key_of_identifier = Context.Cache.key_of_identifier

  let identifier_of_key = Context.Cache.identifier_of_key

  let pp fmt ctxt = Context.Cache.pp fmt (context ctxt)

  let find c k = Context.Cache.find (context c) k

  let set_cache_layout c layout =
    Context.Cache.set_cache_layout (context c) layout >>= fun ctxt ->
    Lwt.return (update_context c ctxt)

  let update c k v = Context.Cache.update (context c) k v |> update_context c

  let sync c ~cache_nonce =
    Context.Cache.sync (context c) ~cache_nonce >>= fun ctxt ->
    Lwt.return (update_context c ctxt)

  let clear c = Context.Cache.clear (context c) |> update_context c

  let list_keys c ~cache_index =
    Context.Cache.list_keys (context c) ~cache_index

  let key_rank c key = Context.Cache.key_rank (context c) key

  let cache_size_limit c ~cache_index =
    Context.Cache.cache_size_limit (context c) ~cache_index

  let cache_size c ~cache_index =
    Context.Cache.cache_size (context c) ~cache_index

  let future_cache_expectation c ~time_in_blocks =
    Context.Cache.future_cache_expectation (context c) ~time_in_blocks
    |> update_context c
end



let to_string ctxt = 
    "Ctxt(" ^
    "Timestamp " ^ (Time.to_notation ctxt.timestamp) ^
    "| Level " ^ (Int32.to_string ctxt.level) ^
    "| first_level " ^ (Int32.to_string ctxt.first_level) ^
    ")"
