let metric_routes =
  [
    Dream.get "/throughput" (fun _ -> Dream.html "")
    (* Measures transactions per second.*);
    Dream.get "/latency-stats" (fun _ -> Dream.html "")
    (* Average, min, max latencies.*);
    Dream.get "/consensus-rate" (fun _ -> Dream.html "")
    (* Speed of reaching consensus.*);
    Dream.get "/error-rate" (fun _ -> Dream.html "")
    (* Rate of failed transactions or consensus failures.*);
    Dream.get "/node-uptime" (fun _ -> Dream.html "")
    (* Uptime stats for each node.*);
    Dream.get "/resource-usage" (fun _ -> Dream.html "")
    (* CPU, memory, network I/O per node.*);
    Dream.get "/block-size" (fun _ -> Dream.html "")
    (* Average, min, max block sizes.*);
    Dream.get "/tx-pool" (fun _ -> Dream.html "")
    (* Current size and rate of the transaction pool.*);
    Dream.get "/fork-rate" (fun _ -> Dream.html "")
    (* Frequency of forks in the blockchain.*);
    Dream.get "/custom-metric/:metric" (fun _ -> Dream.html "")
    (* Check for specific metric in all nodes *);
  ]

let other_routes =
  [
    Dream.post "/inject-failure" (fun _ -> Dream.html "")
    (* Introduce controlled failures to test resilience.*);
    Dream.post "/scale" (fun _ -> Dream.html "")
    (* Dynamically adds/removes nodes or clients.*);
    Dream.post "/pause-resume" (fun _ -> Dream.html "")
    (* Pauses and resumes node or client activities.*);
    Dream.post "/latency" (fun _ -> Dream.html "")
    (* Simulates network latency between nodes.*);
    Dream.post "/partition" (fun _ -> Dream.html "")
    (* Emulates network partitions for split-brain tests.*);
    Dream.get "/metrics" (fun _ -> Dream.html "")
    (* Custom metrics (e.g., transaction rate, block time).*);
    Dream.post "/snapshot" (fun _ -> Dream.html "")
    (* Takes a snapshot of current state for later analysis.*);
    Dream.post "/trigger-event" (fun _ -> Dream.html "")
    (* Custom events like smart contract deployments.*);
    Dream.get "/queries" (fun _ -> Dream.html "")
    (* Complex queries on the blockchain state.*);
    Dream.post "/batch-commands" (fun _ -> Dream.html "")
    (* Executes a batch of commands across nodes or clients.*);
  ]

let main_routes =
  let open Test_handler in
  [
    Dream.get "/status" (fun _ -> status_handler ())
    (* Retrieves the health and statistics of each node and client.*);
    Dream.post "/start-test" start_test_handler;
    (* Accepts parameters like protocol type, number of nodes, clients, and their configurations.*)
    Dream.post "/stop-test" start_test_handler;
    (* Stops test started with /start-test.*)
    Dream.get "/protocol-parameters/:name" (fun request ->
        Dream.param request "name" |> protocol_parameters_handler);
    (* Get the mockup parameters from the protocol_folder.*)
    Dream.get "/protocols" (fun _ -> available_protocols ());
    (* Returns the name, hash and path of available protocols *)
    Dream.post "/swap-protocol" (fun _ -> Dream.html "");
    (* Changes the consensus protocol on-the-fly without restarting nodes.*)
    Dream.post "/stop-node/:id" (fun request ->
        Dream.param request "id" |> int_of_string |> stop_node);
    (*Stops a specific node*)
    Dream.get "/nodes" (fun _ -> nodes_handler ()) (*Gets info about the nodes*);
  ]

let () =
  Dream.run @@ Dream.logger
  @@ Dream.router (main_routes @ other_routes @ metric_routes)
