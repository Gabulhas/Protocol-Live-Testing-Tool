(*

TO Remember: You don't want the stamp or whatever since the nonce and the whole header make up the stamp ;)
*)

type contents = {
  target : Target_repr.t;
  nonce : Int64.t;
  miner : Account_repr.t; [@encoding Account_repr.encoding]
}
[@@deriving encoding]

type protocol_data = contents

module Header =
  Custom_protocol_helper.Header_make.MakeHeader
    (struct
      type t = contents

      let encoding = contents_encoding
    end)
    (struct
      type t = contents

      let fake_protocol_data =
        {
          target = Target_repr.zero;
          nonce = 0L;
          miner = Signature.Public_key_hash.zero;
        }

      let encoding = contents_encoding
    end)

include Header

(*

*)

(*
type contents = {
  target : Target_repr.t;
  nonce : int64; [@encoding int64]
  miner : Account_repr.t; [@encoding Account_repr.encoding]
}

type protocol_data = contents


type t = {shell : Block_header.shell_header; protocol_data : contents}

type block_header = t

type raw = Block_header.t

type shell_header = Block_header.shell_header

let raw_encoding = Block_header.encoding

let shell_header_encoding = Block_header.shell_header_encoding

let contents_encoding =
  let open Data_encoding in
  def "block_header.custom.encoding"
  @@ conv
       (fun {target; nonce; miner} -> (target, nonce, miner))
       (fun (target, nonce, miner) -> {target; nonce; miner})
       (obj3
          (req "target" Data_encoding.z)
          (req "nonce" int64)
          (req "miner" Account_repr.encoding))

let protocol_data_encoding = contents_encoding

let raw {shell; protocol_data} =
  let protocol_data =
    Data_encoding.Binary.to_bytes_exn protocol_data_encoding protocol_data
  in
  {Block_header.shell; protocol_data}

let unsigned_encoding =
  let open Data_encoding in
  merge_objs Block_header.shell_header_encoding contents_encoding

let encoding =
  let open Data_encoding in
  def "block_header.custom.full_header"
  @@ conv
       (fun {shell; protocol_data} -> (shell, protocol_data))
       (fun (shell, protocol_data) -> {shell; protocol_data})
       (merge_objs Block_header.shell_header_encoding protocol_data_encoding)

(** Constants *)

let max_header_length =
  let fake_shell =
    {
      Block_header.level = 0l;
      proto_level = 0;
      predecessor = Block_hash.zero;
      timestamp = Time.of_seconds 0L;
      validation_passes = 0;
      operations_hash = Operation_list_list_hash.zero;
      fitness = Fitness_repr.to_raw Fitness_repr.zero;
      context = Context_hash.zero;
    }
  and fake_contents =
    {
      target = Target_repr.zero;
      nonce = 0L;
      miner = Signature.Public_key_hash.zero;
    }
  in
  Data_encoding.Binary.length
    encoding
    {shell = fake_shell; protocol_data = fake_contents}

(** Header parsing entry point  *)

let hash_raw = Block_header.hash

let hash {shell; protocol_data} =
  Block_header.hash
    {
      shell;
      protocol_data =
        Data_encoding.Binary.to_bytes_exn protocol_data_encoding protocol_data;
    }

let to_bytes =
  let open Data_encoding in
  Binary.to_bytes_exn encoding

let of_bytes =
  let open Data_encoding in
  Binary.of_bytes_opt encoding

let to_string_json header =
  Format.asprintf
    "%a"
    Data_encoding.Json.pp
    (Data_encoding.Json.construct encoding header)

*)
