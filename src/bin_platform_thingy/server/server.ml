open Format
open Protocol_detection.Detect_available_protocols
open Protocol_detection.Load_protocol_information
open Tyxml.Html
open Pages.Home

let html_to_string = asprintf "%a@." (pp ~indent:true ())

let element_to_string = Format.asprintf "%a" (Tyxml.Html.pp_elt ())

let home_routes =
  [
    Dream.get "/" (fun _ -> Dream.html @@ html_to_string @@ home protocol_names);
    Dream.get "/wizard/render_command_inputs" (fun request ->
        match Dream.query request "number_of_clients" with
        | None -> Dream.html (element_to_string (h1 [txt "Error"]))
        | Some n ->
            let number = int_of_string n in
            Dream.html (element_to_string (render_inputs_for_clients number)));
    Dream.get "/wizard/parameters" (fun request ->
        match Dream.query request "protocol" with
        | None -> Dream.html (element_to_string (h1 [txt "Error"]))
        | Some protocol ->
            let parameters_string =
              match get_protocol_folder_by_name protocol with
              | None ->
                  Printf.sprintf
                    "No mockup parameters found. Make sure you have an example \
                     'parameters.json' file inside 'protocol_information' \
                     directory. (%s) (%s) (%s)"
                    (protocol_info_path protocol)
                    protocol
                    (String.concat ";"
                    @@ List.map
                         (fun (name, folder) ->
                           Printf.sprintf "'%s' -> '%s'" name folder)
                         protocol_folders_and_names)
              | Some folder -> get_mockup_parameters folder
            in

            Dream.html (element_to_string (parameters_area parameters_string)));
  ]

let common_routes =
  [
    Dream.get "/protocol_names" (fun _ ->
        let list_json = "[" ^ String.concat "," protocol_names ^ "]" in
        Dream.json list_json);
    Dream.get
      "/static/**"
      (Dream.static "./_build/default/src/bin_platform_thingy/client");
  ]

let all_routes = home_routes @ common_routes

let () = Dream.run @@ Dream.logger @@ Dream.router all_routes
