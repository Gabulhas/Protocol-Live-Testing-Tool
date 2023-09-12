type protocol_info = {
  protocol_name : string;
  protocol_folder : string;
  protocol_hash : string;
}
[@@deriving encoding, show]

let prefix = "proto_custom_"

let prefix_len = String.length prefix

let protocol_folder_to_name dir =
  String.sub dir prefix_len (String.length dir - prefix_len)

let protocol_infos =
  let folder = "src" in
  if Sys.is_directory folder then
    Sys.readdir folder |> Array.to_list
    |> List.filter (fun dir -> String.starts_with ~prefix dir)
    |> List.map (fun folder_name ->
           let protocol_name = protocol_folder_to_name folder_name in
           let protocol_folder = Filename.concat folder folder_name in
           let protocol_hash =
             (Load_protocol_information.protocol_TEZOS_PROTOCOL protocol_folder)
               .hash
           in
           {protocol_name; protocol_folder; protocol_hash})
  else []

let protocol_info_by_name name =
  List.find (fun {protocol_name; _} -> name = protocol_name) protocol_infos

let protocol_mockup_parameters name =
  let info = protocol_info_by_name name in
  Load_protocol_information.get_mockup_parameters info.protocol_folder
