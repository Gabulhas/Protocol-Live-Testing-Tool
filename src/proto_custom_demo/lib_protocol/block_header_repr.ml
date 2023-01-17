(*

TO Remember: You don't want the stamp or whatever since the nonce and the whole header make up the stamp ;)
 
*)
type contents = {
  time : Time_repr.t; (*TODO: Remove: time is already in the header*)
  target : Z.t; (*Should be a 256 bit integer*)
  nonce : Int64.t; (*Merkle tree*)
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
       (fun {time; target; nonce} -> (time, target, nonce))
       (fun (time, target, nonce) -> {time; target; nonce})
       (obj3
          (req "time" Time_repr.encoding)
          (req "target" Data_encoding.z)
          (req "nonce" int64))

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
    {time = Time_repr.zero; target = Target_repr.zero; nonce = 0L}
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

let to_bytes = let open Data_encoding in Binary.to_bytes_exn encoding 
let of_bytes = let open Data_encoding in Binary.of_bytes_opt encoding 


