open Tyxml.Html

let main_script =
  script ~a:[a_src (Xml.uri_of_string "/static/client.bc.js")] (txt "")

let main_css_framework =
  link
    ~rel:[`Stylesheet]
    ~href:
      "https://cdn.jsdelivr.net/npm/@picocss/pico@1/css/pico.classless.min.css"
    ()

let main_title = "Platform Thingy - "

let base_template title_text content =
  let title = title (main_title ^ title_text |> txt) in
  html (head title [main_css_framework]) (body [content; main_script])
