open Tezos_crypto

let template_dir = "src/lib_protocol_helper/template/"

let lib_protocol_suffix = "lib_protocol"

let templates_suffix = "TEMPLATES"

let create_folder dir =
  if Sys.file_exists dir then raise (Failure "Directory already exists") ;
  Sys.mkdir dir 0o755

let copy_folder src dst =
  let src_dir = src in
  let dst_dir = dst in

  (* Check if the source directory exists *)
  if not (Sys.file_exists src_dir) then
    failwith (Printf.sprintf "Source directory %s does not exist" src_dir) ;

  (* Check if the destination directory exists *)
  if not (Sys.file_exists dst_dir) then create_folder dst_dir ;

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

let replace_in_file filename template replacement =
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
        protocol_env_version,
        _use_lib_protocol_template,
        _use_lib_client_template ) =
    parse_args ()
  in

  let protocol_name_dir = Filename.concat "src" ("proto_" ^ protocol_name) in
  let env_version_string = string_of_int protocol_env_version in
  let protocol_hash =
    protocol_name |> generate_protocol_hash |> Protocol_hash.to_b58check
  in

  create_folder protocol_name_dir ;

  let copy_to_new_dir suffix =
    copy_folder
      (Filename.concat template_dir suffix)
      (Filename.concat protocol_name_dir suffix)
  in

  copy_to_new_dir "" ;
  copy_to_new_dir lib_protocol_suffix ;
  copy_to_new_dir templates_suffix ;

  Filename.concat protocol_name_dir templates_suffix
  |> Sys.readdir
  |> Array.iter (fun filename ->
         let filename =
           List.fold_left
             Filename.concat
             ""
             [protocol_name_dir; templates_suffix; filename]
         in
         replace_in_file filename "$VERSION$" env_version_string ;
         replace_in_file filename "$PROTOCOL_HASH$" protocol_hash) ;

  Printf.printf
    "The protocol hash of your newly bootstrapped protocol is %s\n\
     Your options: Name \"%s\", Version %d\n\
     Created the protocol here: %s\n\
     To compile the protocol, please execute the \"build_protocol.sh\" script \
     present in the directory\n\n"
    protocol_hash
    protocol_name
    protocol_env_version
    protocol_name_dir
