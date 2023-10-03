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


def start_validator(node, validator_address):

    subprocess.Popen(
        ["./octez-baker-custom-poa", "-endpoint", f"http://localhost:{node['rpc']}",
         "run", validator_address],
    )


async def test():
    blocks_to_wait = 5
    protocol_name = "poa"
    parameters = get_protocol_parameters(protocol_name)

    number_of_nodes = 5
    fitness = 0

    initial_validators = parameters["constants"]["initial_validator_set"]

    assert len(initial_validators) <= number_of_nodes
    start_test_response = start_test(
        protocol_name, number_of_nodes, 0, parameters)

    print("Waiting for nodes to fully load")
    last_status = 'stopped'
    while last_status != 'running':
        status_resp = get_status()
        last_status = status_resp["status"]
        sleep(2)

    nodes = get_nodes()

    print(nodes)

    for i in range(len(initial_validators)):
        start_validator(nodes[i], initial_validators[i]["address"])

    shuffle(nodes)
    target_node = nodes[0]
    async with aiohttp.ClientSession() as session:
        tasks = [show_heads(session, target_node)]

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
        # tasks.append(spam_transactions(session, target_node))

        await asyncio.gather(*tasks)


def main():
    asyncio.run(test())


if __name__ == "__main__":
    main()
