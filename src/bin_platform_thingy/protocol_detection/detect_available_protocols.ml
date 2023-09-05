let prefix = "proto_custom"

let prefix_len = String.length prefix

let protocol_folder_to_name dir =
  let beautify_name name =
    name |> String.split_on_char '_'
    |> List.map String.capitalize_ascii
    |> String.concat " " |> String.trim
  in
  String.sub dir prefix_len (String.length dir - prefix_len) |> beautify_name

let protocol_folders_and_names =
  let folder = "src" in
  if Sys.is_directory folder then
    Sys.readdir folder |> Array.to_list
    |> List.filter (fun dir -> String.starts_with ~prefix dir)
    |> List.map (fun folder_name ->
           ( protocol_folder_to_name folder_name,
             Filename.concat folder folder_name ))
  else []

let protocol_folders =
  List.map (fun (_, folder) -> folder) protocol_folders_and_names

let protocol_names = List.map (fun (name, _) -> name) protocol_folders_and_names

let get_protocol_folder_by_name name =
  match List.find_opt (fun (n, _) -> name = n) protocol_folders_and_names with
  | Some (_, f) -> Some f
  | _ -> None
