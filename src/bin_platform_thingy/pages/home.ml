open Tyxml.Html
open Template_common

let home =
  base_template "Home"
  @@ header
       ~a:[a_class ["container"]]
       [
         hgroup
           [
             h1 [txt "Platform Thong"];
             h2 [txt "This is a tool to help testing Blockchain Protocols"];
           ];
       ]
