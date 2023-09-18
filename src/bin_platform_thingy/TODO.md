- Do something for stopping and starting (and reuse) nodes


# Metrics

### Throughput
- **Protocol**: Implement an RPC service to track the number of transactions included in blocks.
- **Platform**: Query this RPC service periodically and calculate transactions per second.

### Latency Stats
- **Protocol**: Log the timestamp of when transactions are received and included in blocks.
- **Platform**: Query these logs to compute average, min, and max latencies.

### Consensus Rate
- **Protocol**: Expose an RPC service that provides timestamps of when consensus was reached.
- **Platform**: Use these timestamps to calculate the speed of reaching consensus.

### Error Rate
- **Protocol**: Log failed transactions and consensus failures.
- **Platform**: Query these logs and calculate the rate of errors.

### Node Uptime
- **Platform**: Use health checks to ping each node periodically and calculate uptime.

### Resource Usage
- **Platform**: Use system-level monitoring tools to gather CPU, memory, and network I/O metrics.

### Block Size
- **Protocol**: Log the size of each block.
- **Platform**: Query this information and calculate average, min, and max sizes.

### Tx Pool
- **Protocol**: Expose the current size and changes in the transaction pool via RPC.
- **Platform**: Query this information periodically.

### Fork Rate
- **Protocol**: Keep track of forks in the blockchain and expose this via RPC.
- **Platform**: Query this information to calculate the frequency of forks.

### Validator Stats
- **Protocol**: Log performance metrics specific to validators.
- **Platform**: Query these logs for metrics like inclusion rate, attestation effectiveness, etc.

I can enforce mandatory RPC services by making them part of the protocol's API contract. This ensures any new protocol version will implement them.

