open Tyxml.Html
open Template_common

let parameters_area text =
  let rows =
    String.fold_left (fun res x -> if x = '\n' then res + 1 else res) 0 text
  in

  div
    ~a:[a_id "parameter-config"]
    [
      label [txt "Parameter Configuration"];
      textarea ~a:[a_name "parameters"; a_rows rows] (txt text);
    ]

let input_select_protocol ?(hx_target = "#parameter-config") protocol_names =
  div
    [
      label ~a:[a_label_for "select"] [txt "Select Protocol"];
      select
        ~a:
          [
            a_name "protocol";
            Unsafe.string_attrib "hx-get" "/wizard/parameters";
            Unsafe.string_attrib "hx-trigger" "change";
            Unsafe.string_attrib "hx-target" hx_target;
          ]
        (option (txt "Select")
        :: List.map
             (fun name -> option ~a:[a_text_value name] (txt name))
             protocol_names);
    ]

let both_select_and_parameters protocol_names protocol_parameters =
  div
    [input_select_protocol protocol_names; parameters_area protocol_parameters]

let clients_area =
  div
    ~a:[a_id "main_div"]
    [
      label [txt "Number of Clients"];
      input
        ~a:
          [
            a_input_type `Number;
            a_name "number_of_clients";
            Unsafe.string_attrib "hx-get" "/wizard/render_command_inputs";
            Unsafe.string_attrib "hx-trigger" "keyup";
            Unsafe.string_attrib "hx-target" "#commands_config";
          ]
        ();
      div ~a:[a_id "commands_config"] [];
    ]

let startup_form protocol_names =
  let input_variables =
    div
      [
        label [txt "Input variables to be used in the commands"];
        textarea
          ~a:[a_name "parameters"]
          (txt "myVar=myValue (can be accessed using $myVar in the commands)");
      ]
  in

  let input_number_nodes =
    div
      [
        label ~a:[a_label_for "nnodes"] [txt "Number of Nodes to launch"];
        input
          ~a:
            [
              a_input_type `Number;
              a_id "nnodes";
              a_name "number_of_nodes";
              a_placeholder "5";
            ]
          ();
      ]
  in

  let start_button = input ~a:[a_input_type `Submit; a_value "Start"] () in

  section
    ~a:[a_id "form"]
    [
      form ~a:[a_action "/wizard/submit"; a_method `Post]
      @@ [
           both_select_and_parameters
             protocol_names
             "Please select protocl first";
           input_number_nodes;
           clients_area;
           input_variables;
           start_button;
         ];
    ]

let home protocol_names =
  base_template "Home"
  @@ main ~a:[a_class ["container"]]
  @@ [
       header
         ~a:[a_class ["container"]]
         [
           hgroup
             [
               h1 [txt "Platform Thing thong"];
               h2 [txt "This is a tool to help testing Blockchain Protocols"];
             ];
         ];
       startup_form protocol_names;
     ]

let render_inputs_for_clients number =
  let inputs =
    div ~a:[a_style "margin-left: 20px;"]
    @@ List.init number (fun i ->
           div
             [
               label [txt (Printf.sprintf "Command for Client #%d" (i + 1))];
               input
                 ~a:
                   [
                     a_input_type `Text;
                     a_name @@ Printf.sprintf "client_command_%d" i;
                   ]
                 ();
             ])
  in
  let title = label [txt "Client Commands (in Bash)"] in

  div [title; inputs]
