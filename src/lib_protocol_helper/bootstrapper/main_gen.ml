open File_utils

let enviornment_location =
  concat_multiple_filename ["src"; "lib_protocol_environment"]

let version_to_t = function
  | v when v < 3 -> 0
  | v when v < 6 -> 3
  | 6 -> 6
  | _ -> 7

(*

let create_main_from_env version =
    let t_version = version_to_t version in
    let enviornment_location = 
        Filename.concat enviornment_location (Printf.sprintf "environment_protocol_T_%d.ml" t_version) in

    (*Generate the main from enviornment*)

*)
