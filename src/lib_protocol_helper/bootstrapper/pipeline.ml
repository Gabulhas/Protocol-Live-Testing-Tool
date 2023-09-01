open Tezos_crypto
open File_utils

let template_dir = "src/lib_protocol_helper/template/"

let lib_protocol_suffix = "lib_protocol"

let templates_suffix = "TEMPLATES"

let generate_protocol_hash name = Protocol_hash.hash_string [name]

let replace_underscore_with_dash = Str.global_replace (Str.regexp "_") "-"

let pipeline
    ( protocol_name,
      protocol_env_version,
      _use_lib_protocol_template,
      _use_lib_client_template ) =
  let protocol_name_dir = Filename.concat "src" ("proto_" ^ protocol_name) in
  let env_version_string = string_of_int protocol_env_version in

  let protocol_hash =
    protocol_name |> generate_protocol_hash |> Protocol_hash.to_b58check
  in

  let protocol_name_dash = replace_underscore_with_dash protocol_name in

  create_folder protocol_name_dir ;

  let copy_to_new_dir suffix =
    copy_folder
      (Filename.concat template_dir suffix)
      (Filename.concat protocol_name_dir suffix)
  in

  copy_to_new_dir "" ;
  copy_to_new_dir lib_protocol_suffix ;
  copy_to_new_dir templates_suffix ;

  append_to_file
    "dune-project"
    (Printf.sprintf
       "(package (name tezos-protocol-%s));; ADDED BY BOOTSTRAPPER"
       protocol_name_dash) ;

  Filename.concat protocol_name_dir templates_suffix
  |> Sys.readdir
  |> Array.iter (fun filename ->
         let filename =
           concat_multiple_filename
             [protocol_name_dir; templates_suffix; filename]
         in
         replace_in_file filename "$VERSION$" env_version_string ;
         replace_in_file filename "$PROTOCOL_HASH$" protocol_hash ;
         replace_in_file filename "$PROTOCOL_NAME$" protocol_name ;
         replace_in_file filename "$PROTOCOL-NAME$" protocol_name_dash) ;

  Printf.printf
    "The protocol hash of your newly bootstrapped protocol is %s. This will \
     identify your protocol among other protocols.\n\
     You can check the TEZOS_PROTOCOL file inside your protocol directory.\n\n\
     Your options: Name \"%s\", Version %d\n\
     Created the protocol here: %s\n\
     To compile the protocol, please execute the \"build_protocol.sh\" script \
     present in the directory\n\n"
    protocol_hash
    protocol_name
    protocol_env_version
    protocol_name_dir ;

  let files_for_manual =
    [
      "Makefile";
      "opam/octez-client.opam";
      "opam/octez-proxy-server.opam";
      "opam/octez-node.opam";
      Printf.sprintf "opam/tezos-embedded-protocol-%s.opam" protocol_name_dash;
      Printf.sprintf "opam/tezos-client-%s.opam" protocol_name_dash;
      "src/bin_client/dune";
      "src/bin_node/dune";
    ]
  in

  Printf.printf
    "Depending of your requirements (if you want a client and/or an embedded \
     protocol) please change and update the following files (look for \
     'custom-demo' or 'custom_demo'): %s\n\
     (Notice: this should be improved in the future)"
    (String.concat "\n-" ("" :: files_for_manual))
