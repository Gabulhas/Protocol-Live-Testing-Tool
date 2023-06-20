let template_dir = "src/lib_protocol_helper/template/"

let lib_protocol_suffix = "lib_protocol"

let templates_suffix = "TEMPLATES"

let copy_folder src dst =
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

let copy_and_replace src_file dst_file template replacement =
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

let parse_args () =
  let protocol_name = ref "" in
  let protocol_env_version = ref 0 in
  let use_lib_protocol_template = ref false in
  let use_lib_client_template = ref false in
  let specs =
    [
      ( "-name",
        Arg.Set_string protocol_name,
        "Protocol name. Should only contain letters, numbers or underscore." );
      ("-env", Arg.Set_int protocol_env_version, "Protocol environment version");
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

let is_valid_name (name : string) =
  let name_len = String.length name in

  let rec is_valid_name_aux i =
    if i == name_len then true
    else
      match name.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' -> is_valid_name_aux (succ i)
      | _ -> false
  in
  is_valid_name_aux 0

let () =
  let ( protocol_name,
        protocol_env_version,
        use_lib_protocol_template,
        use_lib_client_template ) =
    parse_args ()
  in

  if not (is_valid_name protocol_name) then
    Failure
      (Printf.sprintf
         "%s is not a valid name. It Should only contain letters, numbers or \
          underscore."
         protocol_name)
    |> raise ;

  let protocol_name_dir = "src/proto_" ^ protocol_name in

  if true then Failure "NOT IMPLEMENTED YET" |> raise
