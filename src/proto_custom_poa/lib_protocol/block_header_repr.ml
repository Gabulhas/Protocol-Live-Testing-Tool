(*
In order for a node to be added, a client using a validator's key must send a specific operation or something similar
 *)
type error +=
  | (* Permanent *)
      Invalid_block_signature of
      Block_hash.t * Signature.Public_key_hash.t

type contents = {
  validator : Account_repr.t; [@encoding Account_repr.encoding]
  authority_list : Account_repr.t list;
      [@encoding Account_repr.authority_list_encoding]
  vote : Vote_repr.t option;
}
[@@deriving encoding]

type protocol_data = {contents : contents; signature : Signature.t}
[@@deriving encoding]

let contents_to_string p =
  "| Validator: "
  ^ Account_repr.to_b58check p.validator
  ^ "| Authority_list: "
  ^ String.concat "," (List.map Account_repr.to_b58check p.authority_list)
  ^ "| Vote: "

let protocol_data_to_string p =
  Signature.to_b58check p.signature ^ "," ^ contents_to_string p.contents

let fake_contents =
  {validator = Account_repr.zero; authority_list = []; vote = None}

let fake_protocol_data = {contents = fake_contents; signature = Signature.zero}

module Header =
  Custom_protocol_helper.Header_make.MakeHeader
    (struct
      type t = contents

      let encoding = contents_encoding
    end)
    (struct
      type t = protocol_data

      let fake_protocol_data = fake_protocol_data

      let encoding = protocol_data_encoding
    end)

include Header

type block_watermark = Block_header of Chain_id.t

let bytes_of_block_watermark = function
  | Block_header chain_id ->
      Bytes.cat (Bytes.of_string "\x11") (Chain_id.to_bytes chain_id)

let to_watermark b = Signature.Custom (bytes_of_block_watermark b)

let of_watermark = function
  | Signature.Custom b ->
      if Compare.Int.(Bytes.length b > 0) then
        match Bytes.get b 0 with
        | '\x11' ->
            Option.map
              (fun chain_id -> Block_header chain_id)
              (Chain_id.of_bytes_opt (Bytes.sub b 1 (Bytes.length b - 1)))
        | _ -> None
      else None
  | _ -> None

let unsigned_to_bytes =
  let open Data_encoding in
  Binary.to_bytes_exn contents_encoding

let check_signature (block : t) (chain_id : Chain_id.t)
    (key : Signature.Public_key.t) =
  let check_signature key ({shell; protocol_data = {contents; signature}} : t) =
    let unsigned_header =
      Data_encoding.Binary.to_bytes_exn unsigned_encoding (shell, contents)
    in
    Signature.check
      ~watermark:(to_watermark (Block_header chain_id))
      key
      signature
      unsigned_header
  in
  if check_signature key block then ok ()
  else
    error (Invalid_block_signature (hash block, Signature.Public_key.hash key))

let () =
  register_error_kind
    `Permanent
    ~id:"block_header.invalid_block_signature"
    ~title:"Invalid block signature"
    ~description:"A block was not signed with the expected private key."
    ~pp:(fun ppf (block, pkh) ->
      Format.fprintf
        ppf
        "Invalid signature for block %a. Expected: %a."
        Block_hash.pp_short
        block
        Signature.Public_key_hash.pp_short
        pkh)
    Data_encoding.(
      obj2
        (req "block" Block_hash.encoding)
        (req "expected" Signature.Public_key_hash.encoding))
    (function
      | Invalid_block_signature (block, pkh) -> Some (block, pkh) | _ -> None)
    (fun (block, pkh) -> Invalid_block_signature (block, pkh))
