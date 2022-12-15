module H =
  Blake2B.Make
    (Base58)
    (struct
      let name = "Block Hash"

      let title = "Block Hash"

      let b58check_prefix = ""

      let size = Some 64
    end)

(* Define the mining function *)
let mine_block (block : Alpha_context.Block_header.t) (target : int64) :
    int64 option =
  (* Set the initial nonce for the block to 0 *)
  let open Data_encoding in
  let open Compare.Bytes in
  let nonce = Int64.zero in
  let target_bytes = Data_encoding.Binary.to_bytes_exn int64 target in

  let shell = block.shell in
  let contents = block.protocol_data in

  (* Keep generating hashes until we find a hash that is below the target value *)
  let rec loop nonce =
    (* Calculate the hash for the block using the Blake2b hash function *)
    let new_protocol_data = {contents with nonce} in
    let temp_block : Alpha_context.Block_header.t =
      {shell; protocol_data = new_protocol_data}
    in
    let block_hash =
      H.hash_bytes
        [
          Data_encoding.Binary.to_bytes_exn
            Alpha_context.Block_header.encoding
            temp_block;
        ]
    in
    let block_hash_bytes = H.to_bytes block_hash in

    (* If the hash is below the target, we have found a valid block hash *)
    if block_hash_bytes < target_bytes then block_hash_bytes
      (* If the hash is not below the target, increment the nonce and try again *)
    else loop (Int64.succ nonce)
  in
  loop nonce |> Data_encoding.Binary.of_bytes_opt int64

(* Define the function to check if a block's hash is below a target value *)
let is_valid_block_hash block target =
  (* Calculate the hash for the block using the Blake2b hash function *)
  let bytes = block |> Block_header_repr.hash |> Block_hash.to_bytes in
  let word = TzEndian.get_int64 bytes 0 in
  Lwt.return Compare.Uint64.(word <= target)

