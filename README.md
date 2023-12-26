# Tezos Protocol Live testing Tool

## Introduction
This project focuses on the development and testing of blockchain consensus protocols, leveraging the adaptability of the Tezos blockchain. It includes the implementation of two consensus algorithms, Proof of Work (PoW) and Proof of Authority (PoA), and a Live Testing Tool for real-time testing and analysis.

**The tool implemented not only serves to get real time information of metrics (TPS, Time To Consensus, etc), to test the protocol execution overall, but also to easily execute and trying out changes on protocols while in a development workflow.**

This was developed during my thesis, which can be found [here](https://github.com/Gabulhas/thesis-Consensus-Protocol-as-a-Plugin).

## Consensus Protocols

Besides the Live Testing tool, 2 protocols were also implemented. 
This includes both economic protocol, baker and clients.

### Proof of Work (PoW)
A consensus algorithm that requires participants (miners) to solve complex cryptographic puzzles to validate transactions and create new blocks. Similar to Bitcoin's original protocol.

The implementation can be found [here](./src/proto_custom_demo).


### Proof of Authority (PoA)
- **Description**: A permissioned consensus algorithm where a set of trusted nodes (validators) are responsible for validating transactions and creating new blocks.
Utilizes a round-robin mechanism for validator selection. The rounds are defined by the order of the hash of the validator's address + round number.
Basically, for every known validator, it's created an hash using `h(validator_address + round_number)`, and sort by hash. The first on the list is the current validator. If it fails to validate during the round time, the next on the list should be the next validator, and so on.

The implementation can be found [here](./src/proto_custom_poa).

## Live Testing Tool

### Overview
- **Functionality**: A web server-based tool that provides a dynamic environment for testing blockchain protocols. It offers various metrics for performance evaluation and allows control over different network aspects.
- **Design**: Connects to all nodes in the network, continuously pooling information and controlling them. It exposes HTTP endpoints for test program control and network information retrieval.
- **Uses**: As seen [here](./tests_platform_thingy/), it's easy to write a testing script (in any language) to interact with the tool, as the tool exposes an HTTP API for interaction.

### Some Key Endpoints
- `/start-test`, `/stop-test`, `/status`: Control the initiation and termination of test networks.
- `/swap-protocol`, `/change-parameters`: Modify the network's protocol and parameters.
- `/start-node/:id`, `/stop-node/:id`: Manage individual nodes in the network.
- `/tps`, `/time-to-consensus`, `/propagation-times`: Provide metrics like Transactions Per Second and block propagation times.

## Getting Started
Instructions on how to set up and run the consensus protocols and the Live Testing Tool.


Follow the tutorial on [Tezos' official documentation](https://tezos.gitlab.io/introduction/howtoget.html#setting-up-the-development-environment-from-scratch) on how to setup everything for development.

Then all you have to do is execute the build action in the make file.
```
make build
```

## Usage
After building everything, you just need to execute the following command to get the tool up and running.

```
./platform_server
```


If you want to understand how to use this tool, there are examples [here](./tests_platform_thingy).

The main workflow of the testing tool is:
- Start a network with a defined number of nodes (nodes can be added and removed on-demand).
- Setup the bakers (if any) 
- Preform actions
- Retrieve Metrics and other info


## Contributing
I don't expect any contributions unless anyone finds this useful.

Let me know through socials if you ever want to contribute and I'm glad to help.
