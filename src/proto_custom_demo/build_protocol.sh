#!/bin/bash

current_directory=$(pwd)

directory_to_fetch="./lib_protocol"

ocamlfiles=$(ls $directory_to_fetch| grep ml)
module_names=$(echo "$ocamlfiles" | sed 's/.ml.*//g' | uniq | sed 's/.*/\u&/')
quoted_modules=$(printf "$module_names" | sed 's/^/"/' |  sed 's/$/",/')
quoted_modules=${quoted_modules::-1}


rm $directory_to_fetch/TEZOS_PROTOCOL

rm $directory_to_fetch/dune

cp ./TEMPLATES/TEZOS_PROTOCOL $directory_to_fetch/TEZOS_PROTOCOL

cp ./TEMPLATES/lib_protocol:dune $directory_to_fetch/dune

sed -i "s/TEMPLATE_MODULES_PART_QUOTED/${quoted_modules//$'\n'/\\n}/" $directory_to_fetch/TEZOS_PROTOCOL
sed -i "s/TEMPLATE_MODULES_PART/${module_names//$'\n'/\\n}/" $directory_to_fetch/dune
sed -i "s/TEMPLATE_FILES_PART/${ocamlfiles//$'\n'/\\n}/" $directory_to_fetch/dune

cd ../../
eval $(opam env)
make octez-node && make octez-client && make octez-baker-custom-demo
cd $current_directory
