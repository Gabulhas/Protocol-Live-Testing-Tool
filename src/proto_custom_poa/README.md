The protocol hash of your newly bootstrapped protocol is PrxHD7cDaHHgSufj5DLAipNy36eRVMh7wrpKBEoMEsfMYNKcpmy.
This will identify your protocol among other protocols.\n
       
You can check the TEZOS_PROTOCOL file inside your protocol directory.\n\n
Your options: Name \"custom_poa\", Version 6\n
Created the protocol here: src/proto_custom_poa\n
To compile the protocol, please execute the \"build_protocol.sh\" script present in the directory\n\n"


Use the following tutorial to develop your own protocol: 
    https://gabulhas.gitlab.io/tezos-protocol-development-tutorial/


Use the binary `platform_thingy` to test your protocols in a real scenario



    Manually add the following text to the following file: 
    - "src/bin_client/dune" (In case you want to create your own client using Tezos Libs):
         
```dune
;; Search for custom-demo, and add this stanza to the "executable" stanza
(select void_for_linking-tezos-client-custom-poa from
   (tezos-client-custom-poa -> void_for_linking-tezos-client-custom-poa.empty)
   (-> void_for_linking-tezos-client-custom-poa.empty))
;; Add this to the last "rule" stanza 
(write-file void_for_linking-tezos-client-custom-poa.empty "")
```
  

    - "dune-project"
        
```dune
(package (name octez-baker-custom-poa))
(package (name tezos-baker-lib-custom-poa))
(package (name tezos-client-custom-poa))
```

  

Depending of your requirements (if you want a client and/or an embedded protocol) please change and update the following files (look for 'custom-demo' or 'custom_demo'): 
-Makefile
(In case you want to create your own baker binary.)
-opam/octez-client.opam
-opam/octez-proxy-server.opam
-opam/octez-node.opam
-opam/tezos-embedded-protocol-custom-poa.opam
-opam/tezos-client-custom-poa.opam
-src/bin_client/dune
-src/bin_node/dune
(Notice: this should be improved in the future)