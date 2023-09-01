(*
let find_protocol_name line =
  let first_escape = Str.search_forward (regexp_string "\\") line 0 in
  let second_escape =
    Str.search_forward (regexp_string "\\") line (first_escape + 1)
  in
  String.sub line (first_escape + 2) (second_escape - first_escape - 2)

let process_dune_file file_path =
  let lines =
    try
      let ic = open_in file_path in
      let rec read_lines acc =
        try
          let line = input_line ic in
          read_lines (line :: acc)
        with End_of_file ->
          close_in ic ;
          List.rev acc
      in
      read_lines []
    with _ -> []
  in

  let rec aux = function
    | hd :: tl ->
        if Str.string_match (regexp "\"module Name = struct let name .*") hd 0
        then find_protocol_name hd
        else aux tl
    | [] -> ""
  in
  aux lines
let find_protocol_names () =
  let open Filename in
  let folder = "src" in

  if Sys.is_directory folder then
    let proto_folders =
      Sys.readdir folder |> Array.to_list
      |> List.filter (fun dir -> String.starts_with ~prefix:"proto_" dir)
    in
    List.fold_left
      (fun result proto_folder ->
        let dune_path =
          List.fold_left
            concat
            ""
            [folder; proto_folder; "lib_protocol"; "dune"]
        in
        if Sys.file_exists dune_path then process_dune_file dune_path :: result
        else result)
      []
      proto_folders
  else []

*)

let protocol_names =
  let folder = "src" in

  let beautify_name name =
    name |> String.split_on_char '_'
    |> List.map String.capitalize_ascii
    |> String.concat " "
  in

  if Sys.is_directory folder then
    let prefix = "proto_custom" in
    let prefix_len = String.length prefix in

    Sys.readdir folder |> Array.to_list
    |> List.filter (fun dir -> String.starts_with ~prefix dir)
    |> List.map (fun dir ->
           String.sub dir prefix_len (String.length dir - prefix_len)
           |> beautify_name)
    |> List.sort String.compare
  else []
