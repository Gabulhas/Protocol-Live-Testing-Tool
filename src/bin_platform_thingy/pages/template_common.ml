open Tyxml.Html

let main_script =
  script ~a:[a_src (Xml.uri_of_string "/static/client.bc.js")] (txt "")

let htmx_import =
  (*<script src="https://unpkg.com/htmx.org@1.9.5" integrity="sha384-xcuj3WpfgjlKF+FXhSQFQ0ZNr39ln+hwjN3npfM9VBnUskLolQAcN80McRIVOPuO" crossorigin="anonymous"></script>*)
  script
    ~a:[a_src (Xml.uri_of_string "https://unpkg.com/htmx.org@1.9.5")]
    (txt "")

let icons_script =
  script
    ~a:[a_src (Xml.uri_of_string "https://unpkg.com/feather-icons")]
    (txt "")

let icons_script_replace = script (txt "feather.replace();")

let main_css_framework =
  link
    ~rel:[`Stylesheet]
    ~href:
      "https://cdn.jsdelivr.net/npm/@picocss/pico@1/css/pico.classless.min.css"
    ()

let main_title = "Platform Thingy - "

let main_navbar =
  let configure_button =
    details
      ~a:[a_role ["list"]; a_dir `Rtl]
      (summary
         ~a:
           [
             a_aria "haspopup" ["listbox"]; a_role ["link"]; a_class ["contrast"];
           ]
         [txt "Configurations"])
      [
        ul
          ~a:[a_role ["listbox"]]
          [
            li [a ~a:[a_href "#"] [txt "Configure Nodes"]];
            li [a ~a:[a_href "#"] [txt "Configure Network"]];
            li [a ~a:[a_href "#"] [txt "Configure Platform Thingy"]];
          ];
      ]
  in

  nav
    [
      ul [li [strong [txt "Platform thing"]]];
      txt " ";
      ul
        [
          li [a ~a:[a_href "#"] [txt "Dashboard"]];
          li [a ~a:[a_href "#"] [txt "Statistics"]];
          li [configure_button];
        ];
    ]

let base_template title_text content =
  let title = title (main_title ^ title_text |> txt) in
  html
    (head title [main_css_framework])
    (body
       [
         icons_script;
         htmx_import;
         main_navbar;
         content;
         main_script;
         icons_script_replace;
       ])
