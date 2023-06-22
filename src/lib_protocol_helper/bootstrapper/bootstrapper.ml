open Tezos_crypto

let _template_hash = "PsSWgZdC8N49eiNMrL5WYqA3ukvwRud3Y7uHTGNHrcLwEvfGpMn"

let template_dir = "src/lib_protocol_helper/template/"

let lib_protocol_suffix = "lib_protocol"

let templates_suffix = "TEMPLATES"

let copy_folder src dst =
  Printf.printf "Copying %s to %s?" src dst ;

  let src_dir = src in
  let dst_dir = dst in

  (* Check if the source directory exists *)
  if not (Sys.file_exists src_dir) then
    failwith (Printf.sprintf "Source directory %s does not exist" src_dir) ;

  (* Check if the destination directory exists *)
  if not (Sys.file_exists dst_dir) then
    failwith (Printf.sprintf "Destination directory %s does not exist" dst_dir) ;

  (* Get the list of files in the source directory *)
  let files = Sys.readdir src_dir in

  (* Copy each file from the source directory to the destination directory *)
  Array.iter
    (fun file ->
      let src_file = Filename.concat src_dir file in
      let dst_file = Filename.concat dst_dir file in

      (* Check if the file exists and is not a directory *)
      if Sys.file_exists src_file && not (Sys.is_directory src_file) then (
        let ic = open_in src_file in
        let oc = open_out dst_file in

        (* Copy the contents of the file *)
        try
          while true do
            output_char oc (input_char ic)
          done
        with End_of_file ->
          close_in ic ;
          close_out oc))
    files

let _replace_in_file filename template replacement =
  let ic = open_in filename in
  let oc = open_out (filename ^ ".tmp") in
  try
    while true do
      let line = input_line ic in
      let new_line =
        Str.global_replace (Str.regexp_string template) replacement line
      in
      output_string oc (new_line ^ "\n")
    done
  with End_of_file ->
    close_in ic ;
    close_out oc ;
    Sys.rename (filename ^ ".tmp") filename

(*
let _copy_and_replace src_file dst_file template replacement =
  let ic = open_in src_file in
  let oc = open_out dst_file in
  try
    while true do
      let line = input_line ic in
      let new_line =
        Str.global_replace (Str.regexp_string template) replacement line
      in
      output_string oc (new_line ^ "\n")
    done
  with End_of_file ->
    close_in ic ;
    close_out oc
*)

let is_valid_name (name : string) =
  let name_len = String.length name in

  let rec is_valid_name_aux i =
    if i == name_len then true
    else
      match name.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> is_valid_name_aux (succ i)
      | _ -> false
  in
  is_valid_name_aux 0

let parse_args () =
  let protocol_name = ref "" in
  let protocol_env_version = ref 0 in
  let use_lib_protocol_template = ref false in
  let use_lib_client_template = ref false in
  let specs =
    [
      ( "-name",
        Arg.String
          (fun s ->
            if is_valid_name s && s != "" then protocol_name := s
            else raise (Failure "Invalid protocol name.")),
        "Protocol name. Should only contain letters, numbers or underscore." );
      ( "-env",
        Arg.Int
          (fun s ->
            if s < 1 && s > 9 then raise (Failure "Invalid Protocol version")
            else protocol_env_version := s),
        "Protocol environment version" );
      ( "-use-lib-protocol-template",
        Arg.Set use_lib_protocol_template,
        "Use lib_protocol template" );
      ( "-use-lib-client-template",
        Arg.Set use_lib_client_template,
        "Use lib_client template" );
    ]
  in
  Arg.parse specs (fun _ -> ()) "Usage: bootstrapper [options]" ;

  ( !protocol_name,
    !protocol_env_version,
    !use_lib_protocol_template,
    !use_lib_client_template )

let generate_protocol_hash name = Protocol_hash.hash_string [name]

let () =
  let ( protocol_name,
        _protocol_env_version,
        _use_lib_protocol_template,
        _use_lib_client_template ) =
    parse_args ()
  in

  let protocol_name_dir = "src/proto_" ^ protocol_name ^ "/" in

  Printf.printf
    "Protocol hash %s"
    (protocol_name |> generate_protocol_hash |> Protocol_hash.to_string) ;

  copy_folder template_dir protocol_name_dir ;
  copy_folder
    (template_dir ^ lib_protocol_suffix)
    (protocol_name_dir ^ lib_protocol_suffix) ;
  copy_folder
    (template_dir ^ templates_suffix)
    (protocol_name_dir ^ templates_suffix)
