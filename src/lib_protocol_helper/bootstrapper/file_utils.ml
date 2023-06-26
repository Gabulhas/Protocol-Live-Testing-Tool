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

let concat_multiple_filename = List.fold_left Filename.concat ""

let append_to_file filename content =
  let out_channel =
    open_out_gen [Open_creat; Open_text; Open_append] 0o666 filename
  in
  output_string out_channel content ;
  close_out out_channel
