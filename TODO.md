!!!NOTICE!!!

Remember, the point is to test in a real way, not just "swapping"

Note: You can actually control the behaviour of a node using octez-client-admin binary


# POW UPDATES
Note: Difficulty adjustment isn't working
Note: Add "Block size " and "Transaction count" to PoW
Note: Can't add multiple transactions to the same block

# Test Live Algorithms (Aacabar até dia 3)
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


# TESE
- Falar do bin_tps que já vem da tezos
- Falar de outros simuladores e testardores (Mittens)
- Falar da ferramenta que gera protocolos!!!! Isto explicará porque é que fizemos o PoW
- Falar do bin_platform_thingy
