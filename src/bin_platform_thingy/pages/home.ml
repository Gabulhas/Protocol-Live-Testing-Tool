open Tyxml.Html
open Template_common

let startup_form protocol_names =
  let input_number_nodes =
    [
      label ~a:[a_label_for "nnodes"] [txt "Number of Nodes to launch"];
      input
        ~a:
          [
            a_input_type `Number;
            a_id "nnodes";
            a_name "Number of nodes";
            a_placeholder "5";
          ]
        ();
    ]
  in

  let input_select_protocol =
    [
      label ~a:[a_label_for "select"] [txt "Select Protocol"];
      select
        ~a:[a_id "select"; a_name "select"; a_required ()]
        (List.map (fun name -> option (txt name)) protocol_names);
    ]
  in

  let start_button =
    [
      input
        ~a:
          [
            a_input_type `Submit;
            a_value "Start";
            a_onclick "event.preventDefault()";
          ]
        ();
    ]
  in

  section
    ~a:[a_id "form"]
    [form @@ input_number_nodes @ input_select_protocol @ start_button]

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
