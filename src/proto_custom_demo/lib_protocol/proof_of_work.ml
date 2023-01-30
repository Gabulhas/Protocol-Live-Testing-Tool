
type error += InvalidBlockHash


let () =
  register_error_kind
    `Permanent
    ~id:"block.not_lower"
    ~title:"Invalid Block"
    ~description:"The block currently has a hash that is higher than the target"
    ~pp:(fun ppf () ->
      Format.pp_print_string
        ppf
        "Block hash is invalid (or not lower than)")
    Data_encoding.unit
    (function InvalidBlockHash -> Some () | _ -> None)
    (fun () -> InvalidBlockHash )

let is_valid_header block target_bytes =
    Compare.Bytes.(<=) (Block_header_repr.hash block |> Block_hash.to_bytes) target_bytes

let check_block (block: Block_header_repr.t) target =
    match (Target_repr.to_bytes target) with
    | Some a -> (if is_valid_header block a then Lwt.return (ok (())) else fail InvalidBlockHash)
    | None -> fail InvalidBlockHash


