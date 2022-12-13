type operation =
  | Transaction of {
      source : Account_repr.t;
      amount : Tez_repr.tez;
      destination : Account_repr.t;
      fee: Tez_repr.tez;
      nonce : int32;
      difficulty : int32;
    }


type error = Negative_value_transfer of Tez_repr.t (* `Temporary *)

open Data_encoding

let operation_repr_encoding =
  union
    [
      case
        (Tag 0)
        ~title:"Transaction"
        (obj6
           (req "source" Account_repr.encoding)
           (req "amount" Tez_repr.encoding)
           (req "destination" Account_repr.encoding)
           (req "fee" Tez_repr.encoding)
           (req "nonce" int32)
           (req "difficulty" int32))
        (function Transaction {source;amount;destination;fee;nonce;difficulty} -> Some (source,amount,destination,fee,nonce,difficulty))
        (fun (source,amount,destination,fee,nonce,difficulty) -> Transaction {source;amount;destination;fee;nonce;difficulty});
    ]

let encoding = operation_repr_encoding
