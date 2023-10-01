from random import shuffle
import requests

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


async def fetch_tps(session):
    while True:
        async with session.get(f"{ADDRESS}/tps") as response:
            tps = await response.text()
            print(f"Current TPS: {tps}")
        await asyncio.sleep(1)  # Adjust the sleep time as needed
