type tezos_protocol_file = {
  expected_env_version : int32;
  hash : string;
  modules : string list;
}
[@@deriving encoding, show]

let read_file_as_string file_location =
  let ic = open_in file_location in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.to_string s

let protocol_info_path protocol_path =
  Filename.concat protocol_path "protocol_info"

let protocol_lib_protocol_path protocol_path =
  Filename.concat protocol_path "lib_protocol"

let protocol_TEZOS_PROTOCOL protocol_path =
  let open Data_encoding in
  Filename.concat (protocol_lib_protocol_path protocol_path) "TEZOS_PROTOCOL"
  |> read_file_as_string |> Json.from_string |> Result.get_ok
  |> Data_encoding.Json.destruct tezos_protocol_file_encoding

let get_mockup_parameters protocol_path =
  let file_location =
    Filename.concat (protocol_info_path protocol_path) "parameters.json"
  in
  read_file_as_string file_location
