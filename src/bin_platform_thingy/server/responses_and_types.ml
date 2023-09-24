type startTest = {
  protocol_name : string;
  n_nodes : int32;
  fitness : int32;
  parameters : Data_encoding.Json.t;
}
[@@deriving encoding, show]

type nodeInfo = {
  id : int;
  process : Lwt_process.process;
  net_port : int;
  rpc_port : int;
  metrics_port : int;
  node_dir : string;
  mutable is_running : bool;
}

type nodeInfoResponse = {
  node_id : int32;
  pid : int32;
  port : int32;
  rpc : int32;
  metrics : int32;
  dir : string;
}
[@@deriving encoding, show]

type splitNetworkRequest = int64 list list [@@deriving encoding, show]

type status = Stopped | Starting | Running [@@deriving encoding {enum}]

type statusResponse = {
  protocol : string;
  status : status;
  nodes : nodeInfoResponse list;
}
[@@deriving encoding]

type fetchHeadResponse = string list list [@@deriving encoding]

let fetchHeadResponseValue f =
  match List.flatten f with h :: _ -> Some h | [] -> None

let node_rpc_baseurl (node : nodeInfo) =
  "http://127.0.0.1:" ^ string_of_int node.rpc_port

module Metrics = struct
  type tpsResponse = {
    per_node : float list;
    median : float;
    max : float;
    min : float;
  }
  [@@deriving encoding]

  type maxAndMinAndAverageResponse = {max : int64; min : int64; average : float}
  [@@deriving encoding]

  type firstAndLastBlockTimesResponse = {first : int64; last : int64}
  [@@deriving encoding]
end

module Block = struct
  (*this is merged*)

  open Tezos_base

  (*Basically I want to ignore the rest, like this
    type header = {
      level : Int32.t;
      proto_level : int32;
      (* uint8 *)
      predecessor : Block_hash.t;
      timestamp : Time.Protocol.t;
      validation_passes : int32; (* uint8 *)
    }


    (**type operation_list_quota = {max_size : int; max_op : int option}**)
    let header_encoding =
      let open Data_encoding in
      conv
        (fun x -> ((), x))
        (fun ((), x) -> x)
        (merge_objs
           unit
           (conv
              (fun {level; proto_level; predecessor; timestamp; validation_passes} ->
                (level, proto_level, predecessor, timestamp, validation_passes))
              (fun (level, proto_level, predecessor, timestamp, validation_passes) ->
                {level; proto_level; predecessor; timestamp; validation_passes})
              (obj5
                 (req "level" int32)
                 (req "proto_level" int32)
                 (req "predecessor" Block_hash.encoding)
                 (req "timestamp" Time.Protocol.encoding)
                 (req "validation_passes" int32))))
  *)

  (* *)

  (*
  type block_metadata = {
    protocol_data : Data_encoding.Json.t;
    test_chain_status : Data_encoding.Json.t;
    max_operations_ttl : Int32.t;
    max_operation_data_length : Int32.t;
    max_block_header_length : Int32.t;
    operation_list_quota : Data_encoding.Json.t;
  }
  [@@deriving encoding]

  type operation = {
    chain_id : Chain_id.t;
    hash : Operation_hash.t;
    shell : Operation.shell_header;
    protocol_data : Data_encoding.Json.t;
    receipt : Data_encoding.Json.t;
  }
  [@@deriving encoding]

  *)
  type header = Block_header.shell_header

  let header_encoding =
    let open Data_encoding in
    let open Tezos_base.Block_header in
    conv
      (fun x -> ((), x))
      (fun ((), x) -> x)
      (merge_objs
         unit
         (conv (fun shell -> shell) (fun shell -> shell) shell_header_encoding))

  type block_info = {
    protocol : string;
    chain_id : string;
    hash : string;
    header : header;
    (*metadata : block_metadata option;*)
    metadata : Data_encoding.Json.t;
    operations : Data_encoding.Json.t list list;
  }
  [@@deriving encoding]
end
