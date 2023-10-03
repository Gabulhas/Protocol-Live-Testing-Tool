open Lwt.Infix
open Utils
open Responses_and_types

let alive_nodes : (int, nodeInfo) Hashtbl.t = Hashtbl.create 10

let program_start_time = Unix.gmtime (Unix.time ())

let current_protocol = ref ""

let current_status = ref Stopped

let current_test_info = ref None

let octez_node = "./octez-node"

let peer_ids = Hashtbl.create 10

(*
let start_node node_number =
  let%lwt process =
    exec_command
      "./src/bin_node/octez-sandboxed-node.sh"
      [string_of_int node_number]
  in

  Lwt_io.printf "Node %d is now running" node_number >>= fun () ->
  Lwt.return (node_number, process)


let boot_up_nodes amount =
  mutable let rec aux_range n = if n >= 0 then n :: (aux_range @@ pred n) else [] in
  let%lwt node_info = Lwt.all @@ List.map start_node (aux_range amount) in
  Lwt.return node_info
*)

let peer_list_from_ids_and_ports ids_and_ports =
  "--no-bootstrap-peers"
  ^ List.fold_left
      (fun res (_, port, _, _) ->
        res ^ " --peer 127.0.0.1:" ^ string_of_int port ^ " ")
      ""
      ids_and_ports
  ^ "--private-mode"

let base_port = 18000

let base_rpc = 19000

let base_metrics = 17000

let id_to_ports id = (id, base_port + id, base_rpc + id, base_metrics + id)

let rec generate_ids_and_ports n =
  if n > 0 then id_to_ports n :: generate_ids_and_ports (pred n) else []

let init_node ?(peer_list = "") ?(node_dir = "") id port rpc metrics n_nodes =
  let peer_list =
    if peer_list = "" then
      peer_list_from_ids_and_ports @@ generate_ids_and_ports n_nodes
    else peer_list
  in

  let node_dir =
    if node_dir = "" then
      Filename.concat
        "/tmp"
        (Printf.sprintf "tezos-%d-%s" id (time_to_string program_start_time))
    else node_dir
  in

  let%lwt () = ensure_dir_exists node_dir in
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
      {|%s run --metrics-addr=:%d --synchronisation-threshold 0 --network "sandbox" --data-dir "%s" %s --sandbox="./scripts/sandbox.json"|}
      octez_node
      metrics
      node_dir
      peer_list
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
      is_running = true;
    }

let boot_up_nodes n_nodes =
  let ids_and_ports = generate_ids_and_ports n_nodes in
  let peer_list = peer_list_from_ids_and_ports ids_and_ports in

  Lwt.all
  @@ List.map
       (fun (id, port, rpc, metrics) ->
         init_node id port rpc metrics n_nodes ~peer_list >>= fun info ->
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

let stop_node id =
  match Hashtbl.find_opt alive_nodes id with
  | None -> Lwt.return @@ Error (Printf.sprintf "couldn't find node %d" id)
  | Some node ->
      Lwt.return
        (Unix.kill node.process#pid Sys.sigkill ;
         node.is_running <- false ;
         Ok (Printf.sprintf "{\"success\": \"killed node %d\"}" id))

let stop_node_handler id =
  let%lwt stopping_result = stop_node id in
  Dream.json @@ Utils.result_to_json_string @@ stopping_result

let start_node_handler id =
  let get_next_possible_id () =
    Hashtbl.fold (fun id _ res -> max id res) alive_nodes (-1)
  in

  let id = if id = -1 then get_next_possible_id () else id in

  let net_port, rpc_port, metrics_port, node_dir =
    match Hashtbl.find_opt alive_nodes id with
    | None ->
        let _, net_port, rpc_port, metrics_port = id_to_ports id in
        (net_port, rpc_port, metrics_port, "")
    | Some n ->
        (*TODO: HMMMMMMMMm will it work? xD*)
        Lwt.async (fun _ ->
            (if n.is_running then stop_node id else Lwt.return (Ok ""))
            >>= fun _ -> Lwt.return_unit) ;
        (n.net_port, n.rpc_port, n.metrics_port, n.node_dir)
  in
  let%lwt new_node_info =
    init_node
      ~node_dir
      id
      net_port
      rpc_port
      metrics_port
      (Hashtbl.length alive_nodes)
  in

  Hashtbl.replace alive_nodes id new_node_info ;
  Dream.json @@ Utils.result_to_json_string
  @@ Ok (Printf.sprintf "Starting node %d " id)

let stop_test () =
  let ids_list = Hashtbl.fold (fun id _ res -> id :: res) alive_nodes [] in
  Lwt.all @@ List.map stop_node ids_list >>= fun _ ->
  current_status := Stopped ;
  Lwt.return_unit

let start_test request_info =
  let current_time = Unix.gmtime (Unix.time ()) in
  current_status := Starting ;
  current_test_info := Some request_info ;

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

  Requests.poll_until_ready
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

  Node_watcher.start_node_watcher nodes_info >>= fun _ -> Lwt.return ()

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
  stop_test () >>= fun _ ->
  Logger.(log INFO @@ Printf.sprintf "Stopping test") >>= fun _ ->
  Dream.json @@ result_to_json_string (Ok "Stopping test")

let swap_protocol_handler request =
  stop_test () >>= fun _ ->
  Lwt.async (fun _ -> start_test_handler request >>= fun _ -> Lwt.return_unit) ;
  Logger.(log INFO @@ Printf.sprintf "Swapping protocol in test") >>= fun _ ->
  Dream.json @@ result_to_json_string (Ok "Swapping Protocol")

let change_parameters request =
  let open Data_encoding in
  let%lwt json_body = Dream.body request in
  match Json.from_string json_body with
  | Error a -> Dream.respond ~status:`Bad_Request (Printf.sprintf "Error %s" a)
  | Ok json -> (
      match !current_test_info with
      | None ->
          Dream.respond
            ~status:`Bad_Request
            "Error can't change parameters if no test has started"
      | Some current_info ->
          let new_info = {current_info with parameters = json} in

          Logger.(log INFO @@ Printf.sprintf "Swapping protocol in test")
          >>= fun _ ->
          stop_test () >>= fun _ ->
          start_test new_info >>= fun _ ->
          Dream.json @@ result_to_json_string (Ok "Swapping Protocol"))

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

let get_self_peer_id node =
  let open Requests in
  if Hashtbl.mem peer_ids node.id then
    Lwt.return @@ Hashtbl.find peer_ids node.id
  else
    let url = path_of_list [node_rpc_baseurl node; "network/self"] in
    let%lwt self_peer_res = fetch_json_type url Data_encoding.string in
    match self_peer_res with
    | Error _ -> Lwt.return ""
    | Ok s -> Lwt.return @@ Utils.strip_quotes s

let change_peer_status status node_id peer =
  let open Requests in
  let node = Hashtbl.find alive_nodes node_id in
  let url = path_of_list [node_rpc_baseurl node; "network/peers"; peer] in
  make_patch_request url (Printf.sprintf "{\"acl\": \"%s\"}" status)

let kick_peer = change_peer_status "ban"

let trust_peer = change_peer_status "trust"

let split_network list_of_list =
  let exclude_others every_peers network =
    (*This should be extremely slow*)
    let to_be_excluded =
      every_peers
      |> List.filter (fun (node_id, _) -> not (List.mem node_id network))
      |> List.map snd
    in
    Lwt.all @@ List.flatten
    @@ List.map
         (fun node -> List.map (fun peer -> kick_peer node peer) to_be_excluded)
         network
  in
  Logger.(log NETWORK "Splitting the network") >>= fun _ ->
  Lwt.all
  @@ List.map
       (fun node_id ->
         let node = Hashtbl.find alive_nodes node_id in
         get_self_peer_id node >>= fun peer_id -> Lwt.return (node.id, peer_id))
       (List.flatten list_of_list)
  >>= fun fetched_peer_ids ->
  List.iter
    (fun (id, peer_id) -> Hashtbl.replace peer_ids id peer_id)
    fetched_peer_ids ;
  Lwt.all
  @@ List.map
       (fun this_network -> exclude_others fetched_peer_ids this_network)
       list_of_list

let split_network_handler request =
  let open Data_encoding in
  let%lwt json_body = Dream.body request in
  match Json.from_string json_body with
  | Error a -> Dream.respond ~status:`Bad_Request (Printf.sprintf "Error %s" a)
  | Ok json ->
      let split_network_lists =
        Json.destruct splitNetworkRequest_encoding json
      in
      let split_network_list_converted =
        List.map (fun l -> List.map Int64.to_int l) split_network_lists
      in

      Lwt.async (fun _ ->
          split_network split_network_list_converted >>= fun _ ->
          Lwt.return_unit) ;
      Dream.respond ~status:`OK "Ok, splitting network"

let rejoin_network () =
  let all_ids = Hashtbl.fold (fun key _ res -> key :: res) alive_nodes [] in
  Logger.(log NETWORK "Rejoining network") >>= fun _ ->
  Lwt.all
  @@ List.map
       (fun id ->
         let node = Hashtbl.find alive_nodes id in
         get_self_peer_id node >>= fun peer -> Lwt.return (id, peer))
       all_ids
  >>= fun id_info_and_selfpeer ->
  let trust_peer_list_lwt node others =
    List.map (fun (_, peer) -> trust_peer node peer) others
  in

  let rec aux previous current rest =
    let current_id, _ = current in
    let result_lwt = trust_peer_list_lwt current_id (previous @ rest) in
    match rest with
    | [] -> [result_lwt]
    | hd :: tl -> result_lwt :: aux (current :: previous) hd tl
  in

  Lwt.all @@ List.flatten
  @@ aux [] (List.hd id_info_and_selfpeer) (List.tl id_info_and_selfpeer)
  >>= fun _ -> Lwt.return_unit

let rejoin_network_handler () =
  Lwt.async (fun _ -> rejoin_network ()) ;
  Dream.respond ~status:`OK "Ok, rejoining network"
