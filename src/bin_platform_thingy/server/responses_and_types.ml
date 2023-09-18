type startTest = {
  protocol_name : string;
  n_nodes : int32;
  fitness : int32;
  parameters : Data_encoding.Json.t;
}
[@@deriving encoding, show]

(*TODO add a watcher Lwt processor to this type *)
type nodeInfo = {
  id : int;
  process : Lwt_process.process;
  net_port : int;
  rpc_port : int;
  metrics_port : int;
  node_dir : string;
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
