(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2019-2020 Nomadic Labs <contact@nomadic-labs.com>           *)
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

open Storage_sigs

module Registered = struct
  let ghost = false
end

module Ghost = struct
  let ghost = true
end

module type ENCODER = sig
  type t

  val of_bytes : key:(unit -> string list) -> bytes -> t tzresult

  val to_bytes : t -> bytes
end

module Make_encoder (V : VALUE) : ENCODER with type t := V.t = struct
  let of_bytes ~key b =
    match Data_encoding.Binary.of_bytes_opt V.encoding b with
    | None -> error (Raw_context.Storage_error (Corrupted_data (key ())))
    | Some v -> Ok v

  let to_bytes v =
    match Data_encoding.Binary.to_bytes_opt V.encoding v with
    | Some b -> b
    | None -> Bytes.empty
end

let len_name = "len"

let data_name = "data"

let encode_len_value bytes =
  let length = Bytes.length bytes in
  Data_encoding.(Binary.to_bytes_exn int31) length

let decode_len_value key len =
  match Data_encoding.(Binary.of_bytes_opt int31) len with
  | None -> error (Raw_context.Storage_error (Corrupted_data key))
  | Some len -> ok len

module Make_subcontext (R : REGISTER) (C : Raw_context.T) (N : NAME) :
  Raw_context.T with type t = C.t = struct
  type t = C.t

  let to_key k = N.name @ k

  let mem t k = C.mem t (to_key k)

  let mem_tree t k = C.mem_tree t (to_key k)

  let get t k = C.get t (to_key k)

  let get_tree t k = C.get_tree t (to_key k)

  let find t k = C.find t (to_key k)

  let find_tree t k = C.find_tree t (to_key k)

  let add t k v = C.add t (to_key k) v

  let add_tree t k v = C.add_tree t (to_key k) v

  let init t k v = C.init t (to_key k) v

  let init_tree t k v = C.init_tree t (to_key k) v

  let update t k v = C.update t (to_key k) v

  let update_tree t k v = C.update_tree t (to_key k) v

  let add_or_remove t k v = C.add_or_remove t (to_key k) v

  let add_or_remove_tree t k v = C.add_or_remove_tree t (to_key k) v

  let remove_existing t k = C.remove_existing t (to_key k)

  let remove_existing_tree t k = C.remove_existing_tree t (to_key k)

  let remove t k = C.remove t (to_key k)

  let list t ?offset ?length k = C.list t ?offset ?length (to_key k)

  let fold ?depth t k ~order ~init ~f =
    C.fold ?depth t (to_key k) ~order ~init ~f

  let config t = C.config t

  module Tree = C.Tree
  module Proof = C.Proof

  let verify_tree_proof = C.verify_tree_proof

  let verify_stream_proof = C.verify_stream_proof

  let equal_config = C.equal_config

  let project = C.project

  let absolute_key c k = C.absolute_key c (to_key k)

  type error += Block_quota_exceeded = C.Block_quota_exceeded

  type error += Operation_quota_exceeded = C.Operation_quota_exceeded


  let description =
    let description =
      if R.ghost then Storage_description.create () else C.description
    in
    Storage_description.register_named_subcontext description N.name

  let length = C.length
end

module Make_single_data_storage
    (R : REGISTER)
    (C : Raw_context.T)
    (N : NAME)
    (V : VALUE) : Single_data_storage with type t = C.t and type value = V.t =
struct
  type t = C.t

  type context = t

  type value = V.t

  let mem t = C.mem t N.name

  include Make_encoder (V)

  let get t =
    C.get t N.name >>=? fun b ->
    let key () = C.absolute_key t N.name in
    Lwt.return (of_bytes ~key b)

  let find t =
    C.find t N.name >|= function
    | None -> Result.return_none
    | Some b ->
        let key () = C.absolute_key t N.name in
        of_bytes ~key b >|? fun v -> Some v

  let init t v = C.init t N.name (to_bytes v) >|=? fun t -> C.project t

  let update t v = C.update t N.name (to_bytes v) >|=? fun t -> C.project t

  let add t v = C.add t N.name (to_bytes v) >|= fun t -> C.project t

  let add_or_remove t v =
    C.add_or_remove t N.name (Option.map to_bytes v) >|= fun t -> C.project t

  let remove t = C.remove t N.name >|= fun t -> C.project t

  let remove_existing t = C.remove_existing t N.name >|=? fun t -> C.project t

  let () =
    let open Storage_description in
    let description =
      if R.ghost then Storage_description.create () else C.description
    in
    register_value
      ~get:find
      (register_named_subcontext description N.name)
      V.encoding
    [@@coq_axiom_with_reason "stack overflow in Coq"]
end

module type INDEX = sig
  type t

  include Path_encoding.S with type t := t

  type 'a ipath

  val args : ('a, t, 'a ipath) Storage_description.args
end

module Pair (I1 : INDEX) (I2 : INDEX) : INDEX with type t = I1.t * I2.t = struct
  type t = I1.t * I2.t

  let path_length = I1.path_length + I2.path_length

  let to_path (x, y) l = I1.to_path x (I2.to_path y l)

  let of_path l =
    match Misc.take I1.path_length l with
    | None -> None
    | Some (l1, l2) -> (
        match (I1.of_path l1, I2.of_path l2) with
        | Some x, Some y -> Some (x, y)
        | _ -> None)

  type 'a ipath = 'a I1.ipath I2.ipath

  let args = Storage_description.Pair (I1.args, I2.args)
end

module Make_data_set_storage (C : Raw_context.T) (I : INDEX) :
  Data_set_storage with type t = C.t and type elt = I.t = struct
  type t = C.t

  type context = t

  type elt = I.t

  let inited = Bytes.of_string "inited"

  let mem s i = C.mem s (I.to_path i [])

  let add s i = C.add s (I.to_path i []) inited >|= fun t -> C.project t

  let remove s i = C.remove s (I.to_path i []) >|= fun t -> C.project t

  let clear s = C.remove s [] >|= fun t -> C.project t

  let fold s ~order ~init ~f =
    C.fold ~depth:(`Eq I.path_length) s [] ~order ~init ~f:(fun file tree acc ->
        match C.Tree.kind tree with
        | `Value -> (
            match I.of_path file with None -> assert false | Some p -> f p acc)
        | `Tree -> Lwt.return acc)

  let elements s =
    fold s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

  let () =
    let open Storage_description in
    let unpack = unpack I.args in
    register_value (* TODO fixme 'elements...' *)
      ~get:(fun c ->
        let c, k = unpack c in
        mem c k >>= function true -> return_some true | false -> return_none)
      (register_indexed_subcontext
         ~list:(fun c -> elements c >|= ok)
         C.description
         I.args)
      Data_encoding.bool
    [@@coq_axiom_with_reason "stack overflow in Coq"]
end

module Make_indexed_data_storage (C : Raw_context.T) (I : INDEX) (V : VALUE) :
  Indexed_data_storage with type t = C.t and type key = I.t and type value = V.t =
struct
  type t = C.t

  type context = t

  type key = I.t

  type value = V.t

  include Make_encoder (V)

  let mem s i = C.mem s (I.to_path i [])

  let get s i =
    C.get s (I.to_path i []) >>=? fun b ->
    let key () = C.absolute_key s (I.to_path i []) in
    Lwt.return (of_bytes ~key b)

  let find s i =
    C.find s (I.to_path i []) >|= function
    | None -> Result.return_none
    | Some b ->
        let key () = C.absolute_key s (I.to_path i []) in
        of_bytes ~key b >|? fun v -> Some v

  let update s i v =
    C.update s (I.to_path i []) (to_bytes v) >|=? fun t -> C.project t

  let init s i v =
    C.init s (I.to_path i []) (to_bytes v) >|=? fun t -> C.project t

  let add s i v = C.add s (I.to_path i []) (to_bytes v) >|= fun t -> C.project t

  let add_or_remove s i v =
    C.add_or_remove s (I.to_path i []) (Option.map to_bytes v) >|= fun t ->
    C.project t

  let remove s i = C.remove s (I.to_path i []) >|= fun t -> C.project t

  let remove_existing s i =
    C.remove_existing s (I.to_path i []) >|=? fun t -> C.project t

  let clear s = C.remove s [] >|= fun t -> C.project t

  let fold s ~order ~init ~f =
    C.fold ~depth:(`Eq I.path_length) s [] ~order ~init ~f:(fun file tree acc ->
        C.Tree.to_value tree >>= function
        | Some v -> (
            match I.of_path file with
            | None -> assert false
            | Some path -> (
                let key () = C.absolute_key s file in
                match of_bytes ~key v with
                | Ok v -> f path v acc
                | Error _ -> Lwt.return acc))
        | None -> Lwt.return acc)

  let fold_keys s ~order ~init ~f =
    fold s ~order ~init ~f:(fun k _ acc -> f k acc)

  let bindings s =
    fold s ~order:`Sorted ~init:[] ~f:(fun p v acc ->
        Lwt.return ((p, v) :: acc))

  let keys s =
    fold_keys s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

  let () =
    let open Storage_description in
    let unpack = unpack I.args in
    register_value
      ~get:(fun c ->
        let c, k = unpack c in
        find c k)
      (register_indexed_subcontext
         ~list:(fun c -> keys c >|= ok)
         C.description
         I.args)
      V.encoding
    [@@coq_axiom_with_reason "stack overflow in Coq"]
end

module Make_indexed_subcontext (C : Raw_context.T) (I : INDEX) :
  Indexed_raw_context
    with type t = C.t
     and type key = I.t
     and type 'a ipath = 'a I.ipath = struct
  type t = C.t

  type context = t

  type key = I.t

  type 'a ipath = 'a I.ipath

  let clear t = C.remove t [] >|= fun t -> C.project t

  let fold_keys t ~order ~init ~f =
    C.fold ~depth:(`Eq I.path_length) t [] ~order ~init ~f:(fun path tree acc ->
        match C.Tree.kind tree with
        | `Tree -> (
            match I.of_path path with
            | None -> assert false
            | Some path -> f path acc)
        | `Value -> Lwt.return acc)

  let keys t =
    fold_keys t ~order:`Sorted ~init:[] ~f:(fun i acc -> Lwt.return (i :: acc))

  let err_missing_key key = Raw_context.storage_error (Missing_key (key, Copy))

  let copy t ~from ~to_ =
    let from = I.to_path from [] in
    let to_ = I.to_path to_ [] in
    C.find_tree t from >>= function
    | None -> Lwt.return (err_missing_key from)
    | Some tree -> C.add_tree t to_ tree >|= ok

  let remove t k = C.remove t (I.to_path k [])

  let description =
    Storage_description.register_indexed_subcontext
      ~list:(fun c -> keys c >|= ok)
      C.description
      I.args

  let unpack = Storage_description.unpack I.args

  let pack = Storage_description.pack I.args

  module Raw_context : Raw_context.T with type t = C.t I.ipath = struct
    type t = C.t I.ipath

    let to_key i k = I.to_path i k

    let mem c k =
      let t, i = unpack c in
      C.mem t (to_key i k)

    let mem_tree c k =
      let t, i = unpack c in
      C.mem_tree t (to_key i k)

    let get c k =
      let t, i = unpack c in
      C.get t (to_key i k)

    let get_tree c k =
      let t, i = unpack c in
      C.get_tree t (to_key i k)

    let find c k =
      let t, i = unpack c in
      C.find t (to_key i k)

    let find_tree c k =
      let t, i = unpack c in
      C.find_tree t (to_key i k)

    let list c ?offset ?length k =
      let t, i = unpack c in
      C.list t ?offset ?length (to_key i k)

    let init c k v =
      let t, i = unpack c in
      C.init t (to_key i k) v >|=? fun t -> pack t i

    let init_tree c k v =
      let t, i = unpack c in
      C.init_tree t (to_key i k) v >|=? fun t -> pack t i

    let update c k v =
      let t, i = unpack c in
      C.update t (to_key i k) v >|=? fun t -> pack t i

    let update_tree c k v =
      let t, i = unpack c in
      C.update_tree t (to_key i k) v >|=? fun t -> pack t i

    let add c k v =
      let t, i = unpack c in
      C.add t (to_key i k) v >|= fun t -> pack t i

    let add_tree c k v =
      let t, i = unpack c in
      C.add_tree t (to_key i k) v >|= fun t -> pack t i

    let add_or_remove c k v =
      let t, i = unpack c in
      C.add_or_remove t (to_key i k) v >|= fun t -> pack t i

    let add_or_remove_tree c k v =
      let t, i = unpack c in
      C.add_or_remove_tree t (to_key i k) v >|= fun t -> pack t i

    let remove_existing c k =
      let t, i = unpack c in
      C.remove_existing t (to_key i k) >|=? fun t -> pack t i

    let remove_existing_tree c k =
      let t, i = unpack c in
      C.remove_existing_tree t (to_key i k) >|=? fun t -> pack t i

    let remove c k =
      let t, i = unpack c in
      C.remove t (to_key i k) >|= fun t -> pack t i

    let fold ?depth c k ~order ~init ~f =
      let t, i = unpack c in
      C.fold ?depth t (to_key i k) ~order ~init ~f

    let config c =
      let t, _ = unpack c in
      C.config t

    module Tree = struct
      include C.Tree

      let empty c =
        let t, _ = unpack c in
        C.Tree.empty t
    end

    module Proof = C.Proof

    let verify_tree_proof = C.verify_tree_proof

    let verify_stream_proof = C.verify_stream_proof

    let equal_config = C.equal_config

    let project c =
      let t, _ = unpack c in
      C.project t

    let absolute_key c k =
      let t, i = unpack c in
      C.absolute_key t (to_key i k)

    type error += Block_quota_exceeded = C.Block_quota_exceeded

    type error += Operation_quota_exceeded = C.Operation_quota_exceeded

    let consume_gas c g =
      let t, i = unpack c in
      C.consume_gas t g >>? fun t -> ok (pack t i)

    let check_enough_gas c g =
      let t, _i = unpack c in
      C.check_enough_gas t g

    let description = description

    let length c =
      let t, _i = unpack c in
      C.length t
  end

  module Make_set (R : REGISTER) (N : NAME) :
    Data_set_storage with type t = t and type elt = key = struct
    type t = C.t

    type context = t

    type elt = I.t

    let inited = Bytes.of_string "inited"

    let mem s i = Raw_context.mem (pack s i) N.name

    let add s i =
      Raw_context.add (pack s i) N.name inited >|= fun c ->
      let s, _ = unpack c in
      C.project s

    let remove s i =
      Raw_context.remove (pack s i) N.name >|= fun c ->
      let s, _ = unpack c in
      C.project s

    let clear s =
      fold_keys s ~init:s ~order:`Sorted ~f:(fun i s ->
          Raw_context.remove (pack s i) N.name >|= fun c ->
          let s, _ = unpack c in
          s)
      >|= fun t -> C.project t

    let fold s ~order ~init ~f =
      fold_keys s ~order ~init ~f:(fun i acc ->
          mem s i >>= function true -> f i acc | false -> Lwt.return acc)

    let elements s =
      fold s ~order:`Sorted ~init:[] ~f:(fun p acc -> Lwt.return (p :: acc))

    let () =
      let open Storage_description in
      let unpack = unpack I.args in
      let description =
        if R.ghost then Storage_description.create ()
        else Raw_context.description
      in
      register_value
        ~get:(fun c ->
          let c, k = unpack c in
          mem c k >>= function true -> return_some true | false -> return_none)
        (register_named_subcontext description N.name)
        Data_encoding.bool
      [@@coq_axiom_with_reason "stack overflow in Coq"]
  end

  module Make_map (N : NAME) (V : VALUE) :
    Indexed_data_storage with type t = t and type key = key and type value = V.t =
  struct
    type t = C.t

    type context = t

    type key = I.t

    type value = V.t

    include Make_encoder (V)

    let mem s i = Raw_context.mem (pack s i) N.name

    let get s i =
      Raw_context.get (pack s i) N.name >>=? fun b ->
      let key () = Raw_context.absolute_key (pack s i) N.name in
      Lwt.return (of_bytes ~key b)

    let find s i =
      Raw_context.find (pack s i) N.name >|= function
      | None -> Result.return_none
      | Some b ->
          let key () = Raw_context.absolute_key (pack s i) N.name in
          of_bytes ~key b >|? fun v -> Some v

    let update s i v =
      Raw_context.update (pack s i) N.name (to_bytes v) >|=? fun c ->
      let s, _ = unpack c in
      C.project s

    let init s i v =
      Raw_context.init (pack s i) N.name (to_bytes v) >|=? fun c ->
      let s, _ = unpack c in
      C.project s

    let add s i v =
      Raw_context.add (pack s i) N.name (to_bytes v) >|= fun c ->
      let s, _ = unpack c in
      C.project s

    let add_or_remove s i v =
      Raw_context.add_or_remove (pack s i) N.name (Option.map to_bytes v)
      >|= fun c ->
      let s, _ = unpack c in
      C.project s

    let remove s i =
      Raw_context.remove (pack s i) N.name >|= fun c ->
      let s, _ = unpack c in
      C.project s

    let remove_existing s i =
      Raw_context.remove_existing (pack s i) N.name >|=? fun c ->
      let s, _ = unpack c in
      C.project s

    let clear s =
      fold_keys s ~order:`Sorted ~init:s ~f:(fun i s ->
          Raw_context.remove (pack s i) N.name >|= fun c ->
          let s, _ = unpack c in
          s)
      >|= fun t -> C.project t

    let fold s ~order ~init ~f =
      fold_keys s ~order ~init ~f:(fun i acc ->
          get s i >>= function Error _ -> Lwt.return acc | Ok v -> f i v acc)

    let bindings s =
      fold s ~order:`Sorted ~init:[] ~f:(fun p v acc ->
          Lwt.return ((p, v) :: acc))

    let fold_keys s ~order ~init ~f =
      fold_keys s ~order ~init ~f:(fun i acc ->
          mem s i >>= function false -> Lwt.return acc | true -> f i acc)

    let keys s =
      fold_keys s ~order:`Sorted ~init:[] ~f:(fun p acc ->
          Lwt.return (p :: acc))

    let () =
      let open Storage_description in
      let unpack = unpack I.args in
      register_value
        ~get:(fun c ->
          let c, k = unpack c in
          find c k)
        (register_named_subcontext Raw_context.description N.name)
        V.encoding
      [@@coq_axiom_with_reason "stack overflow in Coq"]
  end

  module Make_carbonated_map (N : NAME) (V : VALUE) :
    Non_iterable_indexed_carbonated_data_storage
      with type t = t
       and type key = key
       and type value = V.t = struct
    type t = C.t

    type context = t

    type key = I.t

    type value = V.t

    include Make_encoder (V)

    let len_name = len_name :: N.name

    let data_name = data_name :: N.name

    let consume_mem_gas c =
      let path_length = List.length (Raw_context.absolute_key c N.name) + 1 in
      Raw_context.consume_gas
        c
        (Storage_costs.read_access ~path_length ~read_bytes:0)

    let existing_size c =
      Raw_context.find c len_name >|= function
      | None -> ok (0, false)
      | Some len -> decode_len_value len_name len >|? fun len -> (len, true)

    let consume_read_gas get c =
      let path_length = List.length (Raw_context.absolute_key c N.name) + 1 in
      get c len_name >>=? fun len ->
      Lwt.return
        ( decode_len_value len_name len >>? fun read_bytes ->
          Raw_context.consume_gas
            c
            (Storage_costs.read_access ~path_length ~read_bytes) )

    let consume_write_gas set c v =
      let bytes = to_bytes v in
      let len = Bytes.length bytes in
      Raw_context.consume_gas c (Storage_costs.write_access ~written_bytes:len)
      >>?= fun c ->
      set c len_name (encode_len_value bytes) >|=? fun c -> (c, bytes)

    let consume_remove_gas del c =
      Raw_context.consume_gas c (Storage_costs.write_access ~written_bytes:0)
      >>?= fun c -> del c len_name

    let mem s i =
      consume_mem_gas (pack s i) >>?= fun c ->
      Raw_context.mem c data_name >|= fun res -> ok (Raw_context.project c, res)

    let get s i =
      consume_read_gas Raw_context.get (pack s i) >>=? fun c ->
      Raw_context.get c data_name >>=? fun b ->
      let key () = Raw_context.absolute_key c data_name in
      Lwt.return (of_bytes ~key b >|? fun v -> (Raw_context.project c, v))

    let find s i =
      consume_mem_gas (pack s i) >>?= fun c ->
      let s, _ = unpack c in
      Raw_context.mem (pack s i) data_name >>= fun exists ->
      if exists then get s i >|=? fun (s, v) -> (s, Some v)
      else return (C.project s, None)

    let update s i v =
      existing_size (pack s i) >>=? fun (prev_size, _) ->
      consume_write_gas Raw_context.update (pack s i) v >>=? fun (c, bytes) ->
      Raw_context.update c data_name bytes >|=? fun c ->
      let size_diff = Bytes.length bytes - prev_size in
      (Raw_context.project c, size_diff)

    let init s i v =
      consume_write_gas Raw_context.init (pack s i) v >>=? fun (c, bytes) ->
      Raw_context.init c data_name bytes >|=? fun c ->
      let size = Bytes.length bytes in
      (Raw_context.project c, size)

    let add s i v =
      let add c k v = Raw_context.add c k v >|= ok in
      existing_size (pack s i) >>=? fun (prev_size, existed) ->
      consume_write_gas add (pack s i) v >>=? fun (c, bytes) ->
      add c data_name bytes >|=? fun c ->
      let size_diff = Bytes.length bytes - prev_size in
      (Raw_context.project c, size_diff, existed)

    let remove s i =
      let remove c k = Raw_context.remove c k >|= ok in
      existing_size (pack s i) >>=? fun (prev_size, existed) ->
      consume_remove_gas remove (pack s i) >>=? fun c ->
      remove c data_name >|=? fun c ->
      (Raw_context.project c, prev_size, existed)

    let remove_existing s i =
      existing_size (pack s i) >>=? fun (prev_size, _) ->
      consume_remove_gas Raw_context.remove_existing (pack s i) >>=? fun c ->
      Raw_context.remove_existing c data_name >|=? fun c ->
      (Raw_context.project c, prev_size)

    let add_or_remove s i v =
      match v with None -> remove s i | Some v -> add s i v

    let () =
      let open Storage_description in
      let unpack = unpack I.args in
      register_value
        ~get:(fun c ->
          let c, k = unpack c in
          find c k >|=? fun (_, v) -> v)
        (register_named_subcontext Raw_context.description N.name)
        V.encoding
      [@@coq_axiom_with_reason "stack overflow in Coq"]
  end
end

module type WRAPPER = sig
  type t

  type key

  val wrap : t -> key

  val unwrap : key -> t option
end
