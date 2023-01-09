type error += Missing_signature (* `Permanent *)

type error += Invalid_signature
(* `Permanent *)
(*TODO: add the thing for errors here*)

type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

let raw_encoding = Operation.encoding

type management_operations_types = 
    TransactionOperation
    | CoinbaseOperation
    | RevealOperation

type management_operation_content =
  (* Transaction between Accounts*)
  | Transaction of {amount : Tez_repr.tez; destination : Account_repr.t}
  (* Reward transaction*)
  | Coinbase of Tez_repr.t
  | Reveal of Signature.public_key

type management_operation = {
  source : Account_repr.t;
  fee : Tez_repr.tez;
  counter : Z.t;
  content : management_operation_content;
}

type operation_content= Management of management_operation

type protocol_data = {content : operation_content; signature : Signature.t option}

type operation = {shell : Operation.shell_header; protocol_data : protocol_data}

open Data_encoding

let management_operation_content_encoding =
  union
    [
      case
        (Tag 0)
        ~title:"Transaction"
        (obj2
           (req "amount" Tez_repr.encoding)
           (req "destination" Account_repr.encoding))
        (function
          | Transaction {amount; destination} -> Some (amount, destination)
          | _ -> None)
        (function amount, destination -> Transaction {amount; destination});
      case
        (Tag 1)
        ~title:"Coinbase"
        (obj1 (req "amount" Tez_repr.encoding))
        (function Coinbase amount -> Some amount | _ -> None)
        (function amount -> Coinbase amount);
      case
        (Tag 2)
        ~title:"Reveal"
        (obj1 (req "public_key" Signature.Public_key.encoding))
        (function Reveal t -> Some t | _ -> None)
        (fun t -> Reveal t);
    ]

let management_operation_encoding =
  let open Data_encoding in
  conv
    (fun {source; fee; counter; content} -> (source, fee, counter, content))
    (fun (source, fee, counter, content) -> {source; fee; counter; content})
    (obj4
       (req "source" Account_repr.encoding)
       (req "fee" Tez_repr.encoding)
       (req "counter" z)
       (req "content" management_operation_content_encoding))

let operation_content_encoding =
  let open Data_encoding in
  union
    [
      case
        (Tag 0)
        ~title:"Management"
        management_operation_encoding
        (function Management t -> Some t)
        (fun t -> Management t);
    ]

let protocol_data_encoding =
  let open Data_encoding in
  conv
    (fun { content; signature } -> (content, signature))
    (fun (content, signature) -> { content; signature })
    (obj2
       (req "content" operation_content_encoding)
       (opt "signature" Signature.encoding))

let operation_encoding =
  let open Data_encoding in
  conv
    (fun { shell; protocol_data } -> (shell, protocol_data))
    (fun (shell, protocol_data) -> { shell; protocol_data })
    (obj2
       (req "shell" Operation.shell_header_encoding)
       (req "protocol_data" protocol_data_encoding))
      

let unsigned_operation_encoding =
def "operation.alpha.unsigned_operation"
@@ merge_objs
     Operation.shell_header_encoding
     (obj1 (req "contents" operation_content_encoding))

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
  | _, None -> error Missing_signature
  | c, Some signature -> check ~watermark:Generic_operation c signature
