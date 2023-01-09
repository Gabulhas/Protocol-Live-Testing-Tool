type t = Signature.Public_key_hash.t

type account = t

type error += Invalid_account_notation of string

include Compare.Make (struct
  type nonrec t = t

  let compare = Signature.Public_key_hash.compare
end)

let to_b58check = Signature.Public_key_hash.to_b58check

let account_of_b58data : Base58.data -> Signature.public_key_hash option =
  function
  | Ed25519.Public_key_hash.Data h -> Some (Signature.Ed25519 h)
  | Secp256k1.Public_key_hash.Data h -> Some (Signature.Secp256k1 h)
  | P256.Public_key_hash.Data h -> Some (Signature.P256 h)
  | _ -> None

let contract_of_b58data data : t option =
  match account_of_b58data data with Some pkh -> Some pkh | None -> None

let of_b58check_gen ~of_b58data s =
  match Base58.decode s with
  | Some data -> (
      match of_b58data data with
      | Some c -> ok c
      | None -> error (Invalid_account_notation s))
  | None -> error (Invalid_account_notation s)

let of_b58check = of_b58check_gen ~of_b58data:contract_of_b58data

let pp ppf = Signature.Public_key_hash.pp ppf

let pp_short ppf = Signature.Public_key_hash.pp_short ppf

let encoding =
  Data_encoding.def "AccountHash" Signature.Public_key_hash.encoding

(* Renamed exports. *)

let of_b58data = contract_of_b58data

let rpc_arg =
  let construct = to_b58check in
  let destruct hash =
    Result.map_error (fun _ -> "Cannot parse contract id") (of_b58check hash)
  in
  RPC_arg.make
    ~descr:"A contract identifier encoded in b58check."
    ~name:"contract_id"
    ~construct
    ~destruct
    ()

module Index = struct
  type nonrec t = t

  let path_length = 1

  let to_path c l =
    let raw_key = Data_encoding.Binary.to_bytes_exn encoding c in
    let (`Hex key) = Hex.of_bytes raw_key in
    key :: l

  let of_path = function
    | [key] ->
        Option.bind
          (Hex.to_bytes (`Hex key))
          (Data_encoding.Binary.of_bytes_opt encoding)
    | _ -> None

  let rpc_arg = rpc_arg

  let encoding = encoding

  let compare = compare
end
