module Name = struct
  let name = "custom-demo"
end

module Environment6 = Tezos_protocol_environment.V6.Make (Name) ()

module Environment7 = Tezos_protocol_environment.V7.Make (Name) ()

open Tezos_protocol_environment 
module MakeHelper (PROTOCOL:) = struct end
