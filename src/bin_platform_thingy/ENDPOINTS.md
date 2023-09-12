Dream.get "/status" (fun _ -> Dream.html "") (* Retrieves the health and statistics of each node and client.*);
Dream.post "/start-test" (fun _ -> Dream.html "") (* Accepts parameters like protocol type, number of nodes, clients, and their configurations.*);
Dream.post "/swap-protocol" (fun _ -> Dream.html "") (* Changes the consensus protocol on-the-fly without restarting nodes.*);
Dream.get "/logs" (fun _ -> Dream.html "") (* Fetch real-time or historical logs for debugging.*);

Dream.post "/inject-failure" (fun _ -> Dream.html "") (* Introduce controlled failures to test resilience.*);
Dream.post "/scale" (fun _ -> Dream.html "") (* Dynamically adds/removes nodes or clients.*);
Dream.post "/pause-resume" (fun _ -> Dream.html "") (* Pauses and resumes node or client activities.*);
Dream.post "/latency" (fun _ -> Dream.html "") (* Simulates network latency between nodes.*);
Dream.post "/partition" (fun _ -> Dream.html "") (* Emulates network partitions for split-brain tests.*);
Dream.get "/metrics" (fun _ -> Dream.html "") (* Custom metrics (e.g., transaction rate, block time).*);
Dream.post "/snapshot" (fun _ -> Dream.html "") (* Takes a snapshot of current state for later analysis.*);
Dream.post "/replay" (fun _ -> Dream.html "") (* Replays events from a snapshot or log.*);
Dream.post "/trigger-event" (fun _ -> Dream.html "") (* Custom events like smart contract deployments.*);
Dream.get "/queries" (fun _ -> Dream.html "") (* Complex queries on the blockchain state.*);
Dream.post "/batch-commands" (fun _ -> Dream.html "") (* Executes a batch of commands across nodes or clients.*);

Dream.get "/throughput" (fun _ -> Dream.html "") (* Measures transactions per second.*);
Dream.get "/latency-stats" (fun _ -> Dream.html "") (* Average, min, max latencies.*);
Dream.get "/consensus-rate" (fun _ -> Dream.html "") (* Speed of reaching consensus.*);
Dream.get "/error-rate" (fun _ -> Dream.html "") (* Rate of failed transactions or consensus failures.*);
Dream.get "/node-uptime" (fun _ -> Dream.html "") (* Uptime stats for each node.*);
Dream.get "/resource-usage" (fun _ -> Dream.html "") (* CPU, memory, network I/O per node.*);
Dream.get "/block-size" (fun _ -> Dream.html "") (* Average, min, max block sizes.*);
Dream.get "/tx-pool" (fun _ -> Dream.html "") (* Current size and rate of the transaction pool.*);
Dream.get "/fork-rate" (fun _ -> Dream.html "") (* Frequency of forks in the blockchain.*);
Dream.get "/validator-stats" (fun _ -> Dream.html "") (* Performance metrics specific to validators.*);
