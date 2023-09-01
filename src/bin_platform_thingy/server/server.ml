open Format
open Protocol_detection
open Tyxml.Html

let html_to_string = asprintf "%a@." (pp ~indent:true ())

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Dream.html @@ html_to_string @@ Pages.Home.home protocol_names);
         Dream.get "/protocol_names" (fun _ ->
             let list_json = "[" ^ String.concat "," protocol_names ^ "]" in
             Dream.json list_json);
         Dream.get
           "/static/**"
           (Dream.static "./_build/default/src/bin_platform_thingy/client");
       ]
