(*

TODO: REMOVE THE MINING FROM HERE
the protocol part should not have this, this should be in bin_node instead


also, use the multicore version instead


 *)
open Lwt

module H =
  Blake2B.Make
    (Base58)
    (struct
      let name = "Block Hash"

      let title = "Block Hash"

      let b58check_prefix = ""

      let size = Some 64
    end)

let double_hash_bytes b =
  b |> Block_header_repr.hash |> Block_hash.to_bytes |> fun a ->
  H.hash_bytes [a] |> H.to_bytes

let bytes_to_string b = b |> Hex.of_bytes |> function `Hex a -> a

(* Define a function for mining a block with a given nonce, which involves hashing the
 * block and checking if the resulting hash satisfies a certain condition (in this case,
 * if the first three characters of the hash are "000"). If a valid block is found,
 * return it as a Some value. Otherwise, return None. *)
let mine_nonce ({shell; protocol_data} : Block_header_repr.t) nonce target :
    int64 option Lwt.t =
  let protocol_data = {protocol_data with nonce} in
  let hash = double_hash_bytes {shell; protocol_data} in
  let open Compare.Int in
  if Bytes.compare hash target <= 0 then Lwt.return (Some nonce)
  else Lwt.return None

let mine_block_loop (block : Block_header_repr.t) target (start, finish) : int64 option t=
  let rec loop b nonce =
  let open Compare.Int64 in

    if nonce >= finish then Lwt.return_none

    else
      mine_nonce b nonce target >>= fun res ->
      match res with
      | Some nonce -> Lwt.return (Some nonce)
      | None -> loop b (Int64.succ nonce)
  in
  loop block start

let get_range s e succ =
  let rec aux a = if Int64.equal a e then [] else a :: aux (succ a) in
  aux s

let succ n = n + 1

let partition_load workers =
  let open Compare.Int64 in
  let open Int64 in
  let chuck_size = Int64.div max_int workers in
  let rec aux i =
    if i < workers then
      let start = Int64.mul i chuck_size in
      let finish = Int64.sub (Int64.add chuck_size start) 1L in
      (start, finish) :: aux (Int64.succ i)
    else []
  in
  aux 0L

(* Define a function for starting the worker threads and running the mining tasks. *)
(*
let mine_blocks block target workers =
  let tasks =
    List.mapi
      (fun _ rng -> mine_block_loop block target rng)
      (partition_load workers)
  in
  Lwt_condition
    

  Lwt.pick tasks >>= fun a ->
  Lwt.return a
*)
let mine_block block target =
    mine_block_loop block target (Int64.min_int, Int64.max_int)


type error += InvalidBlockHash


let invalid_block_hash () = error InvalidBlockHash


let check_block block target =
    match (Target_repr.to_bytes target) with
    | Some a -> Lwt.return (Compare.Bytes.(<=) (double_hash_bytes block) a)
    | None -> raise (Failure "TODO:") 
