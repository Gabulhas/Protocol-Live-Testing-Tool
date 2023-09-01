open Pipeline

exception InvalidProtocolName of string

exception InvalidEnvironmentVersion of int

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
            else raise (InvalidProtocolName s)),
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

let prompt () =
  Printf.printf
    "What do you want to name your protocol? (Should only contain letters, \
     numbers or underscore):" ;
  let protocol_name =
    Scanf.scanf " %s" (fun s ->
        if is_valid_name s && s != "" then s else raise (InvalidProtocolName s))
  in

  Printf.printf
    "Which environent version do you want to use? (Between 1 and 9. \
     Recommended Version: 6):" ;
  let protocol_env_version =
    Scanf.scanf " %d" (fun s ->
        if s < 1 && s > 9 then raise (InvalidEnvironmentVersion s) else s)
  in

  Printf.printf
    "Do you want to use the lib_protocol template (with prebuild types, \
     functions and structure)? [Y/n]" ;
  let use_lib_protocol_template =
    Scanf.scanf " %s" (fun s -> s == "" || s == "y" || s == "Y")
  in

  Printf.printf
    "Do you want to use the lib_client template? (same as lib_protocol but for \
     the client part)? [Y/n]" ;
  let use_lib_client_template =
    Scanf.scanf " %s" (fun s -> s == "" || s == "y" || s == "Y")
  in

  ( protocol_name,
    protocol_env_version,
    use_lib_protocol_template,
    use_lib_client_template )

let () =
  if Array.length Sys.argv < 2 then prompt () |> pipeline
  else parse_args () |> pipeline
