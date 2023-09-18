open Lwt.Infix
open Utils
open Responses_and_types

let alive_nodes : (int, nodeInfo) Hashtbl.t = Hashtbl.create 10

let program_start_time = Unix.gmtime (Unix.time ())

let current_protocol = ref ""

let current_status = ref Stopped

let start_node node_number =
  let%lwt process =
    exec_command
      "./src/bin_node/octez-sandboxed-node.sh"
      [string_of_int node_number]
  in

  Lwt_io.printf "Node %d is now running" node_number >>= fun () ->
  Lwt.return (node_number, process)

(*
let boot_up_nodes amount =
  let rec aux_range n = if n >= 0 then n :: (aux_range @@ pred n) else [] in
  let%lwt node_info = Lwt.all @@ List.map start_node (aux_range amount) in
  Lwt.return node_info
*)

let boot_up_nodes n_nodes =
  let base_port = 18000 in
  let base_rpc = 19000 in
  let base_metrics = 17000 in
  let octez_node = "./octez-node" in
  let rec aux_ids_and_ports n =
    if n >= 0 then
      (n, base_port + n, base_rpc + n, base_metrics + n)
      :: aux_ids_and_ports (pred n)
    else []
  in

  let ids_and_ports = aux_ids_and_ports n_nodes in
  let peers_thing =
    "--no-bootstrap-peers"
    ^ List.fold_left
        (fun res (_, port, _, _) ->
          res ^ " --peer 127.0.0.1:" ^ string_of_int port ^ " ")
        ""
        ids_and_ports
    ^ "--private-mode"
  in

  let init_node id port rpc metrics =
    let node_dir =
      Filename.concat
        "/tmp"
        (Printf.sprintf "tezos-%d-%s" id (time_to_string program_start_time))
    in
    let%lwt () = Lwt_unix.mkdir node_dir 0o755 in
    let config_init_cmd =
      Printf.sprintf
        {|
              %s config init\
              --network "sandbox"\
              --data-dir "%s"\
              --net-addr "127.0.0.1:%d"\
              --rpc-addr "127.0.0.1:%d"\
              --expected-pow "0"\
              --connections "%d"
            |}
        octez_node
        node_dir
        port
        rpc
        n_nodes
    in
    let identity_generate_cmd =
      Printf.sprintf
        "%s identity generate \"0\" --data-dir \"%s\""
        octez_node
        node_dir
    in
    let run_command =
      Printf.sprintf
        {|%s run --metrics-addr=:%d --synchronisation-threshold 0 --network "sandbox" --data-dir "%s" %s --sandbox="./scripts/sandbox.json" |}
        octez_node
        metrics
        node_dir
        peers_thing
    in
    let%lwt _ = exec_command ~wait:true "bash" ["-c"; config_init_cmd] in
    let%lwt _ = exec_command ~wait:true "bash" ["-c"; identity_generate_cmd] in
    let%lwt process = exec_command "bash" ["-c"; run_command] in
    Lwt.return
      {
        id;
        process;
        net_port = port;
        rpc_port = rpc;
        metrics_port = metrics;
        node_dir;
      }
  in

  Lwt.all
  @@ List.map
       (fun (id, port, rpc, metrics) ->
         init_node id port rpc metrics >>= fun info ->
         Logger.log Logger.NODE @@ Printf.sprintf "Started Node %d" id
         >>= fun _ -> Lwt.return info)
       ids_and_ports

let activate_protocol node protocol_hash fitness params_path =
  let import_cmd =
    Printf.sprintf
      "./octez-client -endpoint http://127.0.0.1:%d -base-dir %s import secret \
       key activator \
       unencrypted:edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"
      node.rpc_port
      node.node_dir
  in

  let activate_cmd =
    Printf.sprintf
      "./octez-client --endpoint http://127.0.0.1:%d activate protocol %s with \
       fitness %d and key activator and parameters %s"
      node.rpc_port
      protocol_hash
      fitness
      params_path
  in

  let%lwt _ = exec_command ~wait:true "bash" ["-c"; import_cmd] in
  let%lwt _ = exec_command ~wait:true "bash" ["-c"; activate_cmd] in
  Logger.log Logger.INFO
  @@ Printf.sprintf "Activated protocol %s on node %d" protocol_hash node.id
  >>= fun _ -> Lwt.return_unit

(*Might cause some probelms with not necessarily waiting for it to kill??*)
let stop_test () =
  current_protocol := "" ;
  Hashtbl.iter (fun _ node -> node.process#kill 9) alive_nodes ;
  current_status := Stopped

(*
    TODO: activate the protocol, and also return info about the /tmp/folder for each node
    Check this also https://tezos.gitlab.io/developer/openmetrics.html
*)

let start_test request_info =
  let current_time = Unix.gmtime (Unix.time ()) in
  current_status := Starting ;

  let create_tmp_parameters params =
    let json_str = Data_encoding.Json.to_string params in
    let filename =
      Printf.sprintf "/tmp/tmp_parameters_%s.json" (time_to_string current_time)
    in
    (* Change as needed *)
    let%lwt chan =
      Lwt_io.open_file
        ~flags:[Unix.O_WRONLY; Unix.O_CREAT]
        ~perm:0o644
        ~mode:Lwt_io.Output
        filename
    in
    let%lwt () = Lwt_io.write chan json_str in
    Lwt_io.close chan >>= fun () -> Lwt.return filename
  in

  let protocol_info =
    Protocol_detection.Detect_available_protocols.protocol_info_by_name
      request_info.protocol_name
  in

  let%lwt () =
    Logger.start_new_logger
      Logger.
        {
          program_start = time_to_string program_start_time;
          test_start = time_to_string current_time;
          protocol = request_info.protocol_name;
          nodes = request_info.n_nodes;
          parameters = request_info.parameters;
        }
  in

  create_tmp_parameters request_info.parameters >>= fun tmp_params_path ->
  boot_up_nodes (Int32.to_int request_info.n_nodes) >>= fun nodes_info ->
  List.iter (fun (a : nodeInfo) -> Hashtbl.add alive_nodes a.id a) nodes_info ;

  poll_until_ready
    (Printf.sprintf
       "http://localhost:%d/chains/main/blocks/head"
       (Hashtbl.find alive_nodes 1).rpc_port)
    2.0
  >>= fun _ ->
  activate_protocol
    (Hashtbl.find alive_nodes 1)
    protocol_info.protocol_hash
    (Int32.to_int request_info.fitness)
    tmp_params_path
  >>= fun _ ->
  current_status := Running ;
  (*Node_watcher.start_node_watcher nodes_info >>= fun _ -> *)
  Lwt.return ()

let start_test_handler request =
  let open Data_encoding in
  let%lwt json_body = Dream.body request in
  match Json.from_string json_body with
  | Error a -> Dream.respond ~status:`Bad_Request (Printf.sprintf "Error %s" a)
  | Ok json ->
      let start_test_request = Json.destruct startTest_encoding json in
      Lwt.async (fun _ -> start_test start_test_request) ;
      Dream.respond ~status:`OK "Ok, starting test"

let stop_test_handler () =
  stop_test () ;
  Logger.(log INFO @@ Printf.sprintf "Stopping test") >>= fun _ ->
  Dream.json @@ result_to_json_string (Ok "Stopping test")

let nodes_info_as_response () =
  let info_to_response (n : nodeInfo) : nodeInfoResponse =
    {
      node_id = Int32.of_int n.id;
      pid = Int32.of_int n.process#pid;
      port = Int32.of_int n.net_port;
      rpc = Int32.of_int n.rpc_port;
      metrics = Int32.of_int n.metrics_port;
      dir = n.node_dir;
    }
  in

  Hashtbl.fold (fun _ n res -> info_to_response n :: res) alive_nodes []

let nodes_handler () =
  let infos = nodes_info_as_response () in
  let nodeInfoResponse_list_encoding =
    Data_encoding.list nodeInfoResponse_encoding
  in
  let json =
    Data_encoding.Json.construct nodeInfoResponse_list_encoding infos
  in
  Dream.json (Data_encoding.Json.to_string json)

let stop_node id =
  match Hashtbl.find_opt alive_nodes id with
  | None ->
      Dream.json
      @@ Utils.result_to_json_string
           (Error (Printf.sprintf "couldn't find node %d" id))
  | Some node ->
      node.process#kill 9 ;
      Dream.json (Printf.sprintf "{\"success\": \"killed node %d\"}" id)

let available_protocols () =
  let open Protocol_detection.Detect_available_protocols in
  let protocol_info_list_encoding = Data_encoding.list protocol_info_encoding in

  Dream.json
    (construct_json_to_string protocol_info_list_encoding protocol_infos)

let status_handler () =
  let nodes = nodes_info_as_response () in
  let current_protocol = !current_protocol in
  let current_status = !current_status in

  Dream.json
  @@ construct_json_to_string
       statusResponse_encoding
       {nodes; protocol = current_protocol; status = current_status}

let protocol_parameters_handler protocol_name =
  let open Protocol_detection.Detect_available_protocols in
  Dream.json
  @@
  match
    Data_encoding.Json.from_string (protocol_mockup_parameters protocol_name)
  with
  | Error _ as e -> result_to_json_string e
  | Ok a -> Data_encoding.Json.to_string a
