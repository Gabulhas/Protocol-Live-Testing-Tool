open Tezos_shell_services.Block_services.Empty
open Responses_and_types
open Metrics_and_state
open Utils
open Tezos_crypto
open Lwt.Infix

let chain_path = "/chains/main"

let get_head_hash node =
  let url = path_of_list [node_rpc_baseurl node; chain_path] in
  let%lwt head_response = fetch_json_type url fetchHeadResponse_encoding in
  match head_response with
  | Error e -> Lwt.return_error e
  | Ok h_hash -> (
      match fetchHeadResponseValue h_hash with
      | Some a -> Lwt.return_ok a
      | None -> Lwt.return_error (Failure "Empty head list"))

let get_block_info node block_id =
  let url = path_of_list [node_rpc_baseurl node; chain_path; block_id] in
  fetch_json_type url block_info_encoding

let get_current_head_info node =
  let%lwt head_res = get_head_hash node in
  match head_res with
  | Error e -> Lwt.return_error e
  | Ok head_hash -> (
      let%lwt block_info_res = get_block_info node head_hash in
      match block_info_res with
      | Error e -> Lwt.return_error e
      | Ok i -> Lwt.return_ok i)

let fetch_last_x_blocks node x =
  let open Int32 in
  let rec range_list y =
    if y >= 0l then y :: range_list (Int32.pred y) else []
  in

  let unwrap_all_blocks blocks =
    let rec aux bl result =
      match bl with
      | [] -> Lwt.return_ok result
      | Error b :: _ -> Lwt.return_error b
      | Ok a :: tl -> aux tl (a :: result)
    in

    aux blocks []
  in

  let%lwt head_hash_res = get_current_head_info node in
  match head_hash_res with
  | Error e -> Lwt.return_error e
  | Ok current_head ->
      let blocks_to_fetch =
        Int32.max (Int32.of_int x) current_head.header.shell.level
      in
      let hash_as_string = Block_hash.to_string current_head.hash in

      let%lwt fetched_blocks =
        Lwt.all
        @@ List.map
             (fun n ->
               get_block_info node (hash_as_string ^ "~" ^ Int32.to_string n))
             (range_list blocks_to_fetch)
      in
      unwrap_all_blocks fetched_blocks

let tps_calculate (state : MetricsAndState.t) node =
  let calculate_tps (blocks : (float * float) list) =
    match blocks with
    | [] | [_] -> 0.0 (* Not enough data to calculate TPS *)
    | (_, first_timestamp) :: _ ->
        let _, last_timestamp = List.hd (List.rev blocks) in
        let total_time = last_timestamp -. first_timestamp in
        let total_transactions =
          List.fold_left (fun acc (t, _) -> acc +. t) 0. blocks
        in
        if total_time <= 0.0 then 0.0 else total_transactions /. total_time
  in

  let rec loop () =
    let%lwt fetched_blocks = fetch_last_x_blocks node 30 in
    match fetched_blocks with
    | Error a -> Lwt.return_error a
    | Ok a ->
        let compact_txcount_timestamps =
          List.map
            (fun b ->
              ( float_of_int @@ List.length @@ List.flatten b.operations,
                Int64.to_float
                @@ Tezos_base.Time.Protocol.to_seconds b.header.shell.timestamp
              ))
            a
        in
        let last_tps = calculate_tps compact_txcount_timestamps in
        Array.set state.tps node.id last_tps ;
        Lwt_unix.sleep 10. >>= fun _ -> loop ()
  in

  loop ()

let watch state node = Lwt.all [tps_calculate state node]

let start_node_watcher nodes =
  let%lwt node_watcher_state = MetricsAndState.create (List.length nodes) in

  Lwt.all @@ List.map (watch node_watcher_state) nodes
