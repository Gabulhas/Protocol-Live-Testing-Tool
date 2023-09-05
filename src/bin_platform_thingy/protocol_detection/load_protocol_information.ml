let protocol_info_path protocol_path =
  List.fold_left Filename.concat "" [protocol_path; "protocol_info"]

let get_mockup_parameters protocol_path =
  let file_location =
    Filename.concat (protocol_info_path protocol_path) "parameters.json"
  in
  let ic = open_in file_location in
  let n = in_channel_length ic in
  let s = Bytes.create n in
  really_input ic s 0 n ;
  close_in ic ;
  Bytes.to_string s
