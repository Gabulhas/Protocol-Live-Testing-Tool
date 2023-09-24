open Responses_and_types
open Metrics_and_state
open Utils
open Lwt.Infix
open Requests
open Responses_and_types.Block

let chain_path = "chains/main"

let print_error_on_watcher function_name node e =
  Lwt_io.printf
    "Error on watcher of node %s %d: %s\n"
    function_name
    node.id
    (Printexc.to_string e)

let get_head_hash node =
  let url = path_of_list [node_rpc_baseurl node; chain_path; "blocks"] in
  let%lwt head_response = fetch_json_type url fetchHeadResponse_encoding in
  match head_response with
  | Error e ->
      print_error_on_watcher "get_head_hash" node e >>= fun _ ->
      Lwt.return_error e
  | Ok h_hash -> (
      match fetchHeadResponseValue h_hash with
      | Some a -> Lwt.return_ok a
      | None -> Lwt.return_error (Failure "Empty head list"))

let get_block_info node block_id =
  let url =
    path_of_list [node_rpc_baseurl node; chain_path; "blocks"; block_id]
  in
  fetch_json_type url block_info_encoding

let get_current_head_info node =
  let%lwt head_res = get_head_hash node in
  match head_res with
  | Error e ->
      print_error_on_watcher "get_current_head_info" node e >>= fun _ ->
      Lwt.return_error e
  | Ok head_hash -> (
      let%lwt block_info_res = get_block_info node head_hash in
      match block_info_res with
      | Error e -> Lwt.return_error e
      | Ok i -> Lwt.return_ok i)

let fetch_last_x_blocks node x =
  let rec range_list y =
    if y >= 0l then y :: range_list (Int32.pred y) else []
  in

  let unwrap_all_blocks blocks =
    let rec aux bl result =
      match bl with
      | [] -> Lwt.return_ok result
      | Error b :: _ ->
          print_error_on_watcher "unwrap_all_blocks" node b >>= fun _ ->
          Lwt.return_error b
      | Ok a :: tl -> aux tl (a :: result)
    in

    aux blocks []
  in

  let%lwt head_hash_res = get_current_head_info node in
  match head_hash_res with
  | Error e ->
      print_error_on_watcher "fetch_last_x_blocks" node e >>= fun _ ->
      Lwt.return_error e
  | Ok current_head ->
      let blocks_to_fetch =
        Int32.min (Int32.of_int x) (Int32.succ current_head.header.level)
      in
      let hash_as_string = current_head.hash in

      let%lwt fetched_blocks =
        Lwt.all
        @@ List.map
             (fun n ->
               get_block_info node (hash_as_string ^ "~" ^ Int32.to_string n))
             (range_list blocks_to_fetch)
      in
      unwrap_all_blocks fetched_blocks

let tps_calculate node =
  let calculate_tps (blocks : (int64 * int64) list) =
    let open Int64 in
    match blocks with
    | [] | [_] -> Error (Failure "Not enough data to calculate TPS")
    | (_, last_timestamp) :: _ ->
        let _, first_timestamp = List.hd (List.rev blocks) in
        let total_time = sub last_timestamp first_timestamp in
        let total_transactions =
          List.fold_left (fun acc (t, _) -> add acc t) 0L blocks
        in
        if total_time <= 0L then
          Error
            (Failure
               (Printf.sprintf
                  "Total_time < 0 (%s -. %s ) for total_transactions %s"
                  (Int64.to_string last_timestamp)
                  (Int64.to_string first_timestamp)
                  (Int64.to_string total_transactions)))
        else Ok (Int64.to_float total_transactions /. Int64.to_float total_time)
  in

  let rec loop () =
    let%lwt fetched_blocks = fetch_last_x_blocks node 10 in
    match fetched_blocks with
    | Error a ->
        print_error_on_watcher "tps_calculate" node a >>= fun _ ->
        Lwt.return_error a
    | Ok a -> (
        let compact_txcount_timestamps =
          List.map
            (fun b ->
              ( Int64.of_int @@ List.length @@ List.flatten b.operations,
                Tezos_base.Time.Protocol.to_seconds b.header.timestamp ))
            a
        in
        match calculate_tps compact_txcount_timestamps with
        | Error a -> Lwt.return_error a
        | Ok last_tps ->
            Array.set !metrics_state.tps node.id last_tps ;
            Lwt_unix.sleep 2. >>= fun _ -> loop ())
  in

  loop ()

let detect_new_blocks_watcher node =
  let rec loop last_hash =
    let%lwt current_head_result = get_head_hash node in
    match current_head_result with
    | Error e -> Lwt.return_error e
    | Ok hash ->
        if hash != last_hash then (
          let%lwt block_info_res = get_block_info node hash in
          match block_info_res with
          | Error e -> Lwt.return_error e
          | Ok block_info ->
              MetricsAndState.new_block
                !metrics_state
                block_info
                node.id
                (Utils.timestamp_as_seconds ()) ;
              loop block_info.hash)
        else loop last_hash
  in

  loop ""

let node_watcher node =
  let rec retries_loop retries =
    Lwt.pick [tps_calculate node; detect_new_blocks_watcher node] >>= fun r ->
    match r with
    | Error e ->
        if retries < 10 then
          Lwt_unix.sleep 2. >>= fun _ -> retries_loop (succ retries)
        else
          Lwt_io.printf
            "Stopping watcher on %d with %s\n"
            node.id
            (Printexc.to_string e)
    | Ok _ -> Lwt_io.printf "Impossible"
  in
  retries_loop 10

let start_node_watcher nodes =
  let open Tezos_base.Time.Protocol in
  let%lwt first_head_info_res = get_current_head_info (List.hd nodes) in
  match first_head_info_res with
  | Error e -> Lwt.return_error e
  | Ok first_head_info ->
      let%lwt node_watcher_state =
        MetricsAndState.create
          (List.length nodes)
          (to_seconds first_head_info.header.timestamp)
      in
      metrics_state := node_watcher_state ;
      Lwt_unix.sleep 5. >>= fun _ ->
      Lwt.return_ok @@ Lwt.all @@ List.map node_watcher nodes
