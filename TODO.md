!!!NOTICE!!!

Remember, the point is to test in a real way, not just "swapping"


# Test Live Algorithms (Aacabar até dia 3)
- Develop a user-friendly interface to start N nodes.
- Enable protocol selection and customization in the interface.
     - Design a page for protocol selection.
     - Design a page for parameter selection.
- Implement transactions for testing purposes.
    - Standardize transaction interface across protocols.
    - Create "workers" (to fake users) that create transactions at random (to simulate the transactions) as well as accept some sort of "command executer" (that, for example, enables the creation of miners)
    - Create stored variables (like $1 $2, etc) where the keys of the nodes are stored on
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

 
# Easy Protocol Swapping
   - Integrate the `proto_custom_demo` folder into the Tezos build pipeline.
   - Implement a seamless protocol swapping process.
      - This is actually just stopping the previous nodes and restart them

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

