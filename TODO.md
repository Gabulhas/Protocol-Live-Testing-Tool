# Test Live Algorithms
- Develop a user-friendly interface to start N nodes.
- Enable protocol selection and customization in the interface.
- Implement transactions for testing purposes.
- Standardize transaction interface across protocols.
- Implement a specific transaction type for accounting.
- Implement statistics collection:
 - Consensus complexity per node.
 - Finalization time.
 - Transactions per second (TPS) / Throughput.
 - Scalability analysis.
 - Nodes that fail.
 - Independent protocol-specific statistics.

- Create "workers" (to fake users) that create transactions at random (to simulate the transactions) as well as accept some sort of "command executer" (that, for example, enables the creation of miners)
 
# Make it Practical
   - Develop a web server to serve the above features.
   - Create pages for statistics display.
   - Design a page for protocol selection.
   - Design a page for parameter selection.
   - Implement storage for mapping combinations to statistics.
   - Determine how statistics are calculated and displayed.

# Easy Protocol Swapping
   - Integrate the `proto_custom_demo` folder into the Tezos build pipeline.
   - Implement a seamless protocol swapping process.

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

# To-Do List

- Implement a testing framework:
  - Stress tests, unit tests, integration tests.
- Develop a benchmarking suite:
  - Measure transaction throughput, latency, resource usage.
- Define a standardized interface for protocols.
- Create protocol templates (PoW, PoS, BFT).
- Document your work and provide tutorials.
- Implement automated build and testing (CI/CD).
- Consider visualization tools for protocol behavior.

