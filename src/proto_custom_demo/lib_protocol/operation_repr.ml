type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

let raw_encoding = Operation.encoding

type types_of_operation =
  | Transaction of {
      source : Account_repr.t;
      amount : Tez_repr.tez;
      destination : Account_repr.t;
      fee : Tez_repr.tez;
      nonce : int32;
      difficulty : int32;
    }

(*Maybe add signature*)
type operation = {
  shell : Operation.shell_header;
  protocol_data : types_of_operation;
}

open Data_encoding

let types_of_operation_encoding =
  union
    [ case
        (Tag 0)
        ~title:"Transaction"
        (obj6
           (req "source" Account_repr.encoding)
           (req "amount" Tez_repr.encoding)
           (req "destination" Account_repr.encoding)
           (req "fee" Tez_repr.encoding)
           (req "nonce" int32)
           (req "difficulty" int32)
           )
        (function Transaction {source; amount; destination; fee; nonce; difficulty} -> Some (source, amount, destination, fee, nonce, difficulty))
        (function (source, amount, destination, fee, nonce, difficulty) -> Transaction {source; amount; destination; fee; nonce; difficulty} );
    ]

let operation_encoding =
  obj2
    (req "shell" Operation.shell_header_encoding)
    (req "protocol_data" types_of_operation_encoding)

let encoding = operation_encoding

