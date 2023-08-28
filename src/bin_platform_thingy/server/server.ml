open Format
open Tyxml.Html

let html_to_string = asprintf "%a@." (pp ~indent:true ())

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router
       [
         Dream.get "/" (fun _ -> Dream.html @@ html_to_string @@ Pages.Home.home);
         Dream.get
           "/static/**"
           (Dream.static "./_build/default/src/bin_platform_thingy/client");
       ]
