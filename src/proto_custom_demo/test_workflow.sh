#!/bin/bash
current_directory=$(pwd)

cd ../../
rm -rf /tmp/tezos-node.* || true

SESSION='potato'
MININGKEY='tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU'
OTHERKEY='tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv'
KILLCOMMAND='pgrep -f octez | xargs kill -9 2> /dev/null&& rm -rf /tmp/tezos-* &&tmux kill-session'

tmux -2 new-session -d -s $SESSION

client_folder=""

tmux new-window -t $SESSION:1 -n "Node 1" -d "./src/bin_node/octez-sandboxed-node.sh 1 --connections 1" 

while [ "$client_folder" = "" ]
do
    client_folder=$(find /tmp -type d -name "tezos-node*" 2>/dev/null  | head -n1)
    sleep 1
done


tmux new-window -t $SESSION:2 -n "Node 2" -d "./src/bin_node/octez-sandboxed-node.sh 2"
tmux new-window -t $SESSION:3 -n "Node 3" -d "./src/bin_node/octez-sandboxed-node.sh 3"
tmux new-window -t $SESSION:4 -n "Node 4" -d "./src/bin_node/octez-sandboxed-node.sh 4"
tmux new-window -t $SESSION:5 -n "Node 5" -d "./src/bin_node/octez-sandboxed-node.sh 5"
#tmux new-window -t $SESSION:6 -n "Node 6" -d "./src/bin_node/octez-sandboxed-node.sh 6"
#tmux new-window -t $SESSION:7 -n "Node 7" -d "./src/bin_node/octez-sandboxed-node.sh 7"



node_response=""
while [ -z "$node_response" ]
do
    node_response=$(curl -s "http://localhost:18731/chains/main/blocks/head")
done

tmux new-window -t $SESSION:8 -n "CLIENT" -d "eval \`./src/bin_client/octez-init-sandboxed-client.sh 1\` ; octez-client activate protocol PsSWgZdC8N49eiNMrL5WYqA3ukvwRud3Y7uHTGNHrcLwEvfGpMn with fitness 0 and key activator and parameters src/proto_custom_demo/test_stuff/parameters.json ; octez_client reveal $MININGKEY ; octez_client reveal $OTHERKEY; clear ; printf 'REVALED KEY A (MININGKEY): $MININGKEY | REVEALED KEY B (OTHERKEY): $OTHERKEY\n\n'; export MININGKEY=$MININGKEY ; export OTHERKEY=$OTHERKEY ; export stop='$KILLCOMMAND' ; bash"

sleep 2

tmux new-window -t $SESSION:9 -n "MINER"  -d "echo './octez-baker-custom-demo -base-dir $client_folder -endpoint http://localhost:18731 run $MININGKEY'; bash"


# attaches to client
tmux a -t $SESSION:8

cd $current_directory
