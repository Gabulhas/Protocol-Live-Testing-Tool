type error += Missing_signature (* `Permanent *)
type error += Invalid_signature (* `Permanent *)
(*TODO: add the thing for errors here*)


type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

let raw_encoding = Operation.encoding

type types_of_operation =
  (* Transaction between Accounts*)
  | Transaction of {
      source : Account_repr.t;
      amount : Tez_repr.tez;
      destination : Account_repr.t;
      fee : Tez_repr.tez;
      counter : Z.t;
    }
  (* Reward transaction*)
  | Coinbase of {
      amount : Tez_repr.t;
      destination : Account_repr.t;
      counter : Z.t;
    }


type protocol_data = {
    content: types_of_operation;
    signature: Signature.t option
  }

type operation = {
  shell : Operation.shell_header;
  protocol_data : protocol_data;
}

open Data_encoding

let types_of_operation_encoding =
  union
    [
      case
        (Tag 0)
        ~title:"Transaction"
        (obj5
           (req "source" Account_repr.encoding)
           (req "amount" Tez_repr.encoding)
           (req "destination" Account_repr.encoding)
           (req "fee" Tez_repr.encoding)
           (req "counter" Data_encoding.z)
           )
        (function
          | Transaction {source; amount; destination; fee; counter} ->
              Some (source, amount, destination, fee, counter)
          | _ -> None)
        (function
          | source, amount, destination, fee, counter ->
              Transaction {source; amount; destination; fee; counter});
      case
        (Tag 1)
        ~title:"Coinbase"
        (obj3
           (req "amount" Tez_repr.encoding)
           (req "destination" Account_repr.encoding)
           (req "counter" Data_encoding.z))
        (function
          | Coinbase {amount; destination; counter} ->
              Some (amount, destination, counter)
          | _ -> None)
        (function
          | amount, destination, counter ->
              Coinbase {amount; destination; counter});
    ]


let unsigned_operation_encoding =
    def "operation.alpha.unsigned_operation"
    @@ merge_objs
         Operation.shell_header_encoding
         (obj1 (req "contents" types_of_operation_encoding))


let protocol_data_encoding =
  let open Data_encoding in
  conv
    (fun { content; signature } -> (content, signature))
    (fun (content, signature) -> { content; signature })
    (obj2
       (req "content" types_of_operation_encoding)
       (opt "signature" Signature.encoding))

let operation_encoding =
  let open Data_encoding in
  conv
    (fun { shell; protocol_data } -> (shell, protocol_data))
    (fun (shell, protocol_data) -> { shell; protocol_data })
    (obj2
       (req "shell" Operation.shell_header_encoding)
       (req "protocol_data" protocol_data_encoding))


let encoding = operation_encoding



let check_signature key {shell; protocol_data} =
  let check ~watermark contents signature =
    let unsigned_operation =
      Data_encoding.Binary.to_bytes_exn
        unsigned_operation_encoding
        (shell, contents)
    in
    if Signature.check ~watermark key signature unsigned_operation then Ok ()
    else error Invalid_signature
  in

  match (protocol_data.content, protocol_data.signature) with
  | (_, None) ->
      error Missing_signature
  | (c, Some signature) ->
        check ~watermark:Generic_operation c signature

