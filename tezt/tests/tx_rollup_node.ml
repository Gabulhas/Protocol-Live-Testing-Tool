(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2022 Nomadic Labs <contact@nomadic-labs.com>                *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

(* Testing
   -------
   Component:    Tx_rollup_node
   Invocation:   dune exec tezt/tests/main.exe -- --file tx_rollup_node.ml
   Subject:      Various test scenarios for the Tx rollup node
*)

module Rollup = Rollup.Tx_rollup
module Rollup_node = Rollup_node.Tx_node

let get_block_hash block_json =
  JSON.(block_json |-> "hash" |> as_string) |> return

let node_rpc node service =
  let* opt_get = RPC.Curl.get () in
  match opt_get with
  | None -> return None
  | Some get ->
      let url = Format.asprintf "%s/%s" (Rollup_node.endpoint node) service in
      let* result = get ~url in
      return (Some (result |> JSON.parse ~origin:service))

let get_node_inbox ?(block = "head") node =
  let* json = node_rpc node @@ "block/" ^ block ^ "/proto_inbox" in
  match json with
  | None -> return Rollup.{cumulated_size = 0; contents = []; hash = ""}
  | Some json ->
      let cumulated_size = JSON.(json |-> "cumulated_size" |> as_int) in
      let contents =
        JSON.(json |-> "contents" |> as_list |> List.map as_string)
      in
      let hash = JSON.(json |-> "hash" |> as_string) in
      return Rollup.{cumulated_size; contents; hash}

let get_rollup_parameter_file ~protocol =
  let enable_tx_rollup = [(["tx_rollup_enable"], Some "true")] in
  let base = Either.right (protocol, None) in
  Protocol.write_parameter_file ~base enable_tx_rollup

(* Checks that the configuration is stored and that the required
   fields are present. *)
let test_node_configuration =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: configuration"
    ~tags:["tx_rollup"; "configuration"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      (* Originate a rollup with a given operator *)
      let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_rollup_node =
        Rollup_node.create
          ~rollup_id:tx_rollup_hash
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* filename =
        Rollup_node.config_init tx_rollup_node tx_rollup_hash block_hash
      in
      Log.info "Tx_rollup configuration file was successfully created" ;
      let () =
        let open Ezjsonm in
        let req = ["client-keys"; "rollup-id"] in
        (* TODO: add optional args checks *)
        match from_channel @@ open_in filename with
        | `O fields ->
            List.iter
              (fun k ->
                if List.exists (fun (key, _v) -> String.equal k key) fields then
                  ()
                else Test.fail "unexpected configuration field")
              req
        | _ -> Test.fail "Unexpected configuration format"
      in
      unit)

let init_and_run_rollup_node ~operator node client =
  let*! tx_rollup_hash = Client.Tx_rollup.originate ~src:operator client in
  let* () = Client.bake_for client in
  let* _ = Node.wait_for_level node 2 in
  Log.info "Tx_rollup %s was successfully originated" tx_rollup_hash ;
  let* json = RPC.get_block client in
  let* block_hash = get_block_hash json in
  let tx_node =
    Rollup_node.create
      ~rollup_id:tx_rollup_hash
      ~rollup_genesis:block_hash
      ~operator
      client
      node
  in
  let* _ = Rollup_node.config_init tx_node tx_rollup_hash block_hash in
  let* () = Rollup_node.run tx_node in
  Log.info "Tx_rollup node is now running" ;
  let* () = Rollup_node.wait_for_ready tx_node in
  Lwt.return (tx_rollup_hash, tx_node)

(* Checks that the tx_node is ready after originating an associated
   rollup key. *)
let test_tx_node_origination =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: test if the node is ready"
    ~tags:["tx_rollup"; "ready"; "originate"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let* _tx_node = init_and_run_rollup_node ~operator node client in
      unit)

let check_inbox_equality (i1 : Rollup.inbox) (i2 : Rollup.inbox) =
  Check.(
    (( = )
       i1.cumulated_size
       i2.cumulated_size
       ~error_msg:
         "Cumulated size of inboxes computed by the rollup node should be \
          equal to the cumulated size given by the RPC")
      int) ;
  Check.(
    ( = )
      i1.contents
      i2.contents
      ~error_msg:
        "Content of inboxes computed by the rollup node should be equal to the \
         cumulated size given by the RPC"
      (list string))

(* Checks that an inbox received by the tx_rollup node is well stored
   and available in a percistent way. *)
let test_tx_node_store_inbox =
  Protocol.register_test
    ~__FILE__
    ~title:"TX_rollup: store inbox"
    ~tags:["tx_rollup"; "store"; "inbox"]
    (fun protocol ->
      let* parameter_file = get_rollup_parameter_file ~protocol in
      let* (node, client) =
        Client.init_with_protocol ~parameter_file `Client ~protocol ()
      in
      let operator = Constant.bootstrap1.public_key_hash in
      let*! rollup = Client.Tx_rollup.originate ~src:operator client in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 2 in
      let* json = RPC.get_block client in
      let* block_hash = get_block_hash json in
      let tx_node =
        Rollup_node.create
          ~rollup_id:rollup
          ~rollup_genesis:block_hash
          ~operator
          client
          node
      in
      let* _ = Rollup_node.config_init tx_node rollup block_hash in
      let* () = Rollup_node.run tx_node in
      (* Submit a batch *)
      let batch = "tezos_l2_batch_1" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:batch
          ~rollup
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 3 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 3 in
      let* node_inbox_1 = get_node_inbox ~block:"0" tx_node in
      let*! inbox_1 = Rollup.get_inbox ~rollup ~level:0 client in
      (* Ensure that stored inboxes on daemon's side are equivalent of
         inboxes returned by the rpc call. *)
      check_inbox_equality node_inbox_1 inbox_1 ;
      let snd_batch = "tezos_l2_batch_2" in
      let*! () =
        Client.Tx_rollup.submit_batch
          ~content:snd_batch
          ~rollup
          ~src:Constant.bootstrap1.public_key_hash
          client
      in
      let* () = Client.bake_for client in
      let* _ = Node.wait_for_level node 4 in
      let* _ = Rollup_node.wait_for_tezos_level tx_node 4 in
      let* node_inbox_2 = get_node_inbox ~block:"1" tx_node in
      let*! inbox_2 = Rollup.get_inbox ~rollup ~level:1 client in
      (* Ensure that stored inboxes on daemon side are equivalent of inboxes
         returned by the rpc call. *)
      check_inbox_equality node_inbox_2 inbox_2 ;
      (* Stop the node and try to get the inbox once again*)
      let* () = Rollup_node.terminate tx_node in
      let* () = Rollup_node.run tx_node in
      let* () = Rollup_node.wait_for_ready tx_node in
      let*! inbox_after_restart = Rollup.get_inbox ~rollup ~level:1 client in
      check_inbox_equality node_inbox_2 inbox_after_restart ;
      unit)

let register ~protocols =
  test_node_configuration protocols ;
  test_tx_node_origination protocols ;
  test_tx_node_store_inbox protocols