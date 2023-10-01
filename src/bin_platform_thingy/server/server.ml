let metric_routes =
  let open Metrics_handler in
  [
    Dream.get "/tps" (fun _ -> tps ()) (* Measures transactions per second.*);
    Dream.get "/time-to-consensus" (fun _ -> time_to_consensus ());
    Dream.get "/propagation-times" (fun _ -> propagation_times ());
    Dream.get "/first-and-last-block-times" (fun _ ->
        first_and_last_block_times ());
    Dream.get "/blocks-per-second" (fun _ -> blocks_per_second ());
    Dream.get "/discarded-blocks" (fun _ -> discarded_blocks ());
    Dream.get "/custom-metric/:metric" (fun _ -> Dream.html "");
  ]

let other_routes = []

let main_routes =
  let open Test_handler in
  [
    Dream.get "/status" (fun _ -> status_handler ())
    (* Retrieves the health and statistics of each node and client.*);
    Dream.post "/start-test" start_test_handler;
    (* Accepts parameters like protocol type, number of nodes, clients, and their configurations.*)
    Dream.post "/stop-test" (fun _ -> stop_test_handler ());
    (* Stops test started with /start-test.*)
    Dream.post
      "/swap-protocol"
      swap_protocol_handler (* Swaps current protocol*);
    Dream.get "/protocol-parameters/:name" (fun request ->
        Dream.param request "name" |> protocol_parameters_handler);
    (* Get the mockup parameters from the protocol_folder.*)
    Dream.get "/protocols" (fun _ -> available_protocols ());
    (* Returns the name, hash and path of available protocols *)
    Dream.post "/change-parameters" (fun request -> change_parameters request);
    Dream.get "/nodes" (fun _ -> nodes_handler ()) (*Gets info about the nodes*);
    Dream.get "/start_node/:id" (fun request ->
        Dream.param request "id" |> int_of_string |> start_node_handler);
    Dream.post "/start_node/" (fun _ -> start_node_handler (-1));
    Dream.get "/stop-node/:id" (fun request ->
        Dream.param request "id" |> int_of_string |> stop_node_handler);
    Dream.post "/split-network" split_network_handler;
    Dream.get "/rejoin-network" (fun _ -> rejoin_network_handler ());
  ]

let () = Dream.run @@ Dream.router (main_routes @ other_routes @ metric_routes)
