open Tezos_crypto
open File_utils

let template_dir = "src/lib_protocol_helper/template/"

let lib_protocol_suffix = "lib_protocol"

let templates_suffix = "TEMPLATES"

let generate_protocol_hash name = Protocol_hash.hash_string [name]

let replace_underscore_with_dash = Str.global_replace (Str.regexp "_") "-"

let pipeline (protocol_name, protocol_env_version, _use_lib_protocol_template) =
  let protocol_name = "custom_" ^ protocol_name in
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
       {|
(package (name tezos-protocol-%s));; ADDED BY BOOTSTRAPPER
(package (name tezos-embedded-protocol-%s))\n\
|}
       protocol_name_dash
       protocol_name_dash) ;

  append_to_file "script-inputs/active_protocol_versions" protocol_name_dash ;

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

  let protocol_info_text =
    Printf.sprintf
      {|The protocol hash of your newly bootstrapped protocol is %s.
This will identify your protocol among other protocols.\n
       
You can check the TEZOS_PROTOCOL file inside your protocol directory.\n\n
Your options: Name \"%s\", Version %d\n
Created the protocol here: %s\n
To compile the protocol, please execute the \"build_protocol.sh\" script present in the directory\n\n"


Use the following tutorial to develop your own protocol: 
    https://gabulhas.gitlab.io/tezos-protocol-development-tutorial/


Use the binary `platform_thingy` to test your protocols in a real scenario


|}
      protocol_hash
      protocol_name
      protocol_env_version
      protocol_name_dir
  in

  replace_in_file
    "src/bin_node/dune"
    "$BIN_NODE_1$"
    (Printf.sprintf
       {|
(select void_for_linking-tezos-embedded-protocol-%s from
  (tezos-embedded-protocol-%s -> void_for_linking-tezos-embedded-protocol-%s.empty)
  (-> void_for_linking-tezos-embedded-protocol-%s.empty))

;; FOR BOOTSTRAPPER. DON'T REMOVE $BIN_NODE_1$
  |}
       protocol_name_dash
       protocol_name_dash
       protocol_name_dash
       protocol_name_dash) ;

  replace_in_file
    "src/bin_node/dune"
    "$BIN_NODE_2$"
    (Printf.sprintf
       {|
(write-file void_for_linking-tezos-embedded-protocol-%s.empty "")
;; FOR BOOTSTRAPPER. DON'T REMOVE $BIN_NODE_2$
  |}
       protocol_name_dash) ;

  let bin_client_text =
    Printf.sprintf
      {| 
```dune
;; Search for custom-demo, and add this stanza to the "executable" stanza
(select void_for_linking-tezos-client-%s from
   (tezos-client-%s -> void_for_linking-tezos-client-%s.empty)
   (-> void_for_linking-tezos-client-%s.empty))
;; Add this to the last "rule" stanza 
(write-file void_for_linking-tezos-client-%s.empty "")
```
  |}
      protocol_name_dash
      protocol_name_dash
      protocol_name_dash
      protocol_name_dash
      protocol_name_dash
  in

  let dune_project_text =
    Printf.sprintf
      {|
```dune
(package (name octez-baker-%s))
(package (name tezos-baker-lib-%s))
(package (name tezos-client-%s))
```|}
      protocol_name_dash
      protocol_name_dash
      protocol_name_dash
  in

  let manually_add_test =
    Printf.sprintf
      {|
Manually add the following text to the following file: 
- "src/bin_client/dune" (In case you want to create your own client using Tezos Libs):
    %s

- "dune-project"
    %s

  |}
      bin_client_text
      dune_project_text
  in

  let files_for_manual =
    [
      ("Makefile", "In case you want to create your own baker binary.");
      ("opam/octez-client.opam", "");
      ("opam/octez-proxy-server.opam", "");
      ("opam/octez-node.opam", "");
      ( Printf.sprintf "opam/tezos-embedded-protocol-%s.opam" protocol_name_dash,
        "" );
      (Printf.sprintf "opam/tezos-client-%s.opam" protocol_name_dash, "");
      ("src/bin_client/dune", "");
      ("src/bin_node/dune", "");
    ]
  in

  let depending_text =
    Printf.sprintf
      "\n\n\
       Depending of your requirements (if you want a client and/or an embedded \
       protocol) please change and update the following files (look for \
       'custom-demo' or 'custom_demo'): %s\n\n\
       (Notice: this should be improved in the future)"
      (String.concat
         "\n-"
         (""
         :: List.map
              (fun (file, desc) ->
                Printf.sprintf
                  "%s%s"
                  file
                  (if desc != "" then Printf.sprintf "\n(%s)" desc else ""))
              files_for_manual))
  in

  let last_text =
    "\nAll this info will be stored in your protocol's folder as README.md"
  in

  let full_text = protocol_info_text ^ manually_add_test ^ depending_text in

  print_string (full_text ^ last_text) ;
  append_to_file (Filename.concat protocol_name_dir "README.md") full_text
