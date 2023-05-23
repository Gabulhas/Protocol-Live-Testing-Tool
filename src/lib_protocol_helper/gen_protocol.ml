let gen_protocol_code version =
  Printf.sprintf
    "module Name = struct let name = \"custom-demo\" end\n\
     include Tezos_protocol_environment.V%s.Make(Name)()\n"
    version
