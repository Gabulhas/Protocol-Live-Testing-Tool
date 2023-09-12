!!!NOTICE!!!

Remember, the point is to test in a real way, not just "swapping"

Check this also https://tezos.gitlab.io/developer/openmetrics.html

Also check how to gather a node's config from its rpc or similar

Note: The way that a  protocol updates is in the modules Raw_context.activate or Alpha_context.activate


# Test Live Algorithms (Aacabar até dia 3)
- Develop a user-friendly interface to start N nodes.
- Implement transactions for testing purposes.
    - Standardize transaction interface across protocols.
    - Create "workers" (to fake users) that create transactions at random (to simulate the transactions) as well as accept some sort of "command executer" (that, for example, enables the creation of miners)
    - Detect the custom protocols (maybe by using the "_custom_" in the name) 
    - Use sandbox nodes to startup
- Implement a specific transaction type for accounting.
- Implement statistics collection:
     - Consensus complexity per node.
     - Finalization time.
     - Transactions per second (TPS) / Throughput.
     - Scalability analysis.
     - Nodes that fail.
     - Independent protocol-specific statistics.
    - Implement storage for mapping combinations to statistics.
- Automatically generate info about the protocol, like the parameters 

 

# Protocol Development
   - Identify shared libraries/modules among protocols.
   - Differentiate protocol-specific modules.
   - Key protocol-specific modules to include:
   - `alpha_context.ml` (optional).
   - `apply.ml` (optional).
   - `block_header_repr.ml`.
   - `constants_repr.ml`.
   - `init_storage.ml`.
   - `level_repr.ml`.
   - `main.ml`.
   - `operation_repr.ml`.
   - `parameters_repr.ml`.
   - `raw_context.ml` (optional).
   - `services.ml`.
   - `services_registration.ml`.
   - `storage.ml`.

# Other Ideas
Data Persistence: How will you log or archive test data for later analysis?
API: For external tools to interact with your testing platform, also, so you can code scripts in like python or go to perform more tests (like, running a network for X seconds, retrieve information from each node or from the platform kind of thing)

