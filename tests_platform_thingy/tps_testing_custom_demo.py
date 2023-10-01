import threading
import requests
from random import shuffle
from time import sleep
import subprocess
import asyncio
import aiohttp
import subprocess
import json
import sys

from utils import *

MINER_ADD = "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"
OTHER_ADD = "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv"


def reveal_and_start_mine(node, miner_address):
    subprocess.run(["./octez-client", "--endpoint", "http://127.0.0.1:" + str(node["rpc"]),
                   "reveal", "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU"])

    subprocess.Popen(
        ["./octez-baker-custom-demo", "-base-dir", node["dir"], "-endpoint", f"http://localhost:{node['rpc']}",
         "run", miner_address, "2000000"],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


async def fetch_tps(session):
    while True:
        async with session.get(f"{ADDRESS}/tps") as response:
            tps = await response.text()
            print(f"Current TPS: {tps}")
        await asyncio.sleep(1)  # Adjust the sleep time as needed


async def spam_transactions(session, node):
    node_rpc = node["rpc"]

    while True:
        subprocess.run(
            ["./octez-client", "--endpoint", f"http://127.0.0.1:{node_rpc}", "transfer", "1", "from",
                MINER_ADD, "to", OTHER_ADD],
        )

        await asyncio.sleep(0.2)  # Adjust the sleep time as needed


async def test():
    blocks_to_wait = 5
    protocol_name = "demo"
    parameters = get_protocol_parameters(protocol_name)
    number_of_nodes = 2
    fitness = 0

    start_test_response = start_test(
        protocol_name, number_of_nodes, 0, parameters)

    print("Waiting for nodes to fully load")
    last_status = 'stopped'
    while last_status != 'running':
        status_resp = get_status()
        last_status = status_resp["status"]
        sleep(2)

    pass

    nodes = get_nodes()
    print(nodes)

    print("Mining:")
    reveal_and_start_mine(nodes[0], MINER_ADD)

    target_node = nodes[0]

    async with aiohttp.ClientSession() as session:
        tasks = [fetch_tps(session)]

        print(f"Waiting for level {blocks_to_wait} block")
        last_level = -1
        while True:
            sleep(1)
            last_block_info = get_block_info(
                target_node["rpc"], get_head_hash(target_node["rpc"]))
            level = last_block_info["header"]["level"]

            if last_level != level:
                print(f"Level {level}")
                last_level = level

            if level >= blocks_to_wait:
                break

        # for node in nodes:
        #     tasks.append(spam_transactions(session, node))
        tasks.append(spam_transactions(session, target_node))

        await asyncio.gather(*tasks)


async def monitor_blocks():
    nodes = get_nodes()
    shuffle(nodes)

    node = nodes[0]

    async with aiohttp.ClientSession() as session:
        tasks = [show_heads(session, node)]
        await asyncio.gather(*tasks)


def main():
    if len(sys.argv) > 1 and sys.argv[1] == "monitor":
        asyncio.run(monitor_blocks())
    else:
        asyncio.run(test())


if __name__ == "__main__":
    main()
