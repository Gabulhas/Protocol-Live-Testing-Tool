from pytezos import Key
from utils import *
import subprocess
import asyncio


def pretty_protocol_name(name):
    if name == "poa":
        return "Proof of Authority"
    if name == "demo":
        return "Proof of Work"


def wait_for_blocks(node_rpc, target_blocks):
    print(f"Waiting until {target_blocks} have passed.")
    initial_block = get_head_info(node_rpc)["header"]["level"]
    previous_block = initial_block
    while True:
        current_block = get_head_info(node_rpc)["header"]["level"]
        if previous_block != current_block:
            print("Current level ", current_block)
            previous_block = current_block

        if current_block - initial_block >= target_blocks:
            print(f"Reached target blocks. Stopping test.")
            break
        sleep(1)


def start_bakers(protocol_name, baker_name, bakers, nodes):
    processes = []
    for i in range(len(bakers)):
        node = nodes[i]
        baker = bakers[i]
        if protocol_name == "poa":
            import_key(node, baker["secret_key"], baker["address"])

        process = subprocess.Popen(
            [baker_name, "-base-dir", node["dir"], "-endpoint", f"http://localhost:{node['rpc']}",
             "run", baker["address"]],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        processes.append(process)
    return processes


def stop_bakers(processes):
    for process in processes:
        process.kill()
    for process in processes:
        process.wait()


def start_protocol_test(protocol, n_validators, n_nodes, blocks_to_wait, blocks_to_run, block_time):
    pretty_name = pretty_protocol_name(protocol)

    print(f"Starting {pretty_name} test")

    parameters = get_protocol_parameters(protocol)
    parameters["constants"]["block_time"] = str(block_time)

    baker_name = "./octez-baker-custom-"

    accounts = create_multiple_accounts(n_validators)

    if protocol == "poa":
        parameters["constants"]["initial_validator_set"] = [
            {key: account[key] for key in ('address', 'public_key')}
            for account in accounts
        ]
        baker_name = baker_name + "poa"
    else:
        baker_name = baker_name + "demo"

    print("Starting the network")
    start_test(protocol, n_nodes, 0, parameters)
    wait_until_status("running")

    nodes = get_nodes()

    print("Starting the baker")
    processes = start_bakers(protocol, baker_name, accounts, nodes)

    print("Running the test....")
    wait_for_blocks(nodes[0]['rpc'], blocks_to_run)
    ttc = fetch_time_to_consensus()
    print(f"Final TTC: {ttc}")

    print("Stopping the test....")
    stop_test()
    wait_until_status("stopped")
    stop_bakers(processes)

    print(f"{pretty_name} test complete.")


tests = [
    {"validators": 20, "nodes": 20, "block_time": 30},
    {"validators": 10, "nodes": 10, "block_time": 30},
    {"validators": 2, "nodes": 8, "block_time": 120}
]


blocks_to_wait = 5
blocks_to_run_for = 25

for config in tests:
    for protocol in ["poa", "demo"]:
        start_protocol_test(protocol, config["validators"],
                            config["nodes"], blocks_to_wait, blocks_to_run_for, config["block_time"])
