from random import shuffle
import subprocess
import json
import requests
from pytezos import Key
from time import sleep

ADDRESS = "http://localhost:8080"
SESSION = requests.Session()


def start_test(protocol_name, nodes, fitness, parameters):

    data = {
        "protocol_name": protocol_name,
        "n_nodes": int(nodes),
        "fitness": int(fitness),
        "parameters": parameters
    }
    print(data)

    response = SESSION.post(ADDRESS + "/start-test", json=data)

    if response.status_code == 200:
        print("Success:", response.text)

    else:
        print("Failure:", response.status_code)
        exit(1)


def stop_test():
    return SESSION.post(ADDRESS + "/stop-test").json()


def get_prototocols():
    return SESSION.get(ADDRESS + "/protocols").json()


def get_status():
    return SESSION.get(ADDRESS + "/status").json()


def get_protocol_parameters(protocol_name):
    return SESSION.get(ADDRESS + "/protocol-parameters/" + protocol_name).json()


def get_nodes():
    return SESSION.get(ADDRESS + "/nodes").json()


def get_head_hash(node_rpc):
    return SESSION.get(f"http://127.0.0.1:{node_rpc}/chains/main/blocks").json()[0][0]


def get_head_info(node_rpc):
    return get_block_info(node_rpc, get_head_hash(node_rpc))


def get_block_info(node_rpc, block_id):
    return SESSION.get(f"http://127.0.0.1:{node_rpc}/chains/main/blocks/{block_id}").json()


def get_account_counter(node_rpc, account):

    return int(
        str(SESSION.get(f"http://localhost:{node_rpc}/chains/main/blocks/head/context/account/{account}/counter").text).strip(
        ).removeprefix("\"").removesuffix("\"")
    )


async def show_heads(session, node):
    while True:
        print(json.dumps(get_block_info(
            node["rpc"], get_head_hash(node["rpc"])), indent=2))


async def fetch_tps():
    while True:
        async with SESSION.get(f"{ADDRESS}/tps") as response:
            tps = await response.text()
            print(f"Current TPS: {tps}")
        await asyncio.sleep(1)  # Adjust the sleep time as needed


def fetch_time_to_consensus():
    return (SESSION.get(f"{ADDRESS}/time-to-consensus")).json()


def create_account():
    key = Key.generate(export=False)
    return {"address": key.public_key_hash(),
            "public_key":  key.public_key(),
            "secret_key":  key.secret_key()
            }


def create_multiple_accounts(n):
    accounts = []
    for _ in range(n):
        accounts.append(create_account())

    return accounts


def import_key(node, secret_key, name):
    node_rpc = node["rpc"]
    node_dir = node["dir"]
    subprocess.run(
        ["./octez-client", "--endpoint", f"http://localhost:{node_rpc}", "--base-dir", node_dir, "import", "secret", "key",
            name, "unencrypted:" + str(secret_key)],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL
    )


def wait_until_status(status):
    last_status = ''
    while last_status != status:
        status_resp = get_status()
        last_status = status_resp["status"]
        sleep(1)


def wait_until_level(node_rpc, target_level):
    current_level = 0
    while current_level < target_level:
        current_level = get_head_info(node_rpc)["header"]["level"]
        sleep(1)
