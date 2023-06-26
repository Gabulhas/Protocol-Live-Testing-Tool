open Pipeline

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

let () = parse_args () |> pipeline
