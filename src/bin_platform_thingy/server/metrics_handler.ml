open Metrics_and_state
open Utils
open Responses_and_types.Metrics

let tps () =
  let per_node = Array.to_list !metrics_state.tps in

  let rec calculate_all tps_list max_tps min_tps total =
    match tps_list with
    | [] -> (max_tps, min_tps, total /. float_of_int (List.length per_node))
    | h :: tl ->
        calculate_all
          tl
          (if h > max_tps then h else max_tps)
          (if h < min_tps then h else min_tps)
          (total +. h)
  in

  let max, min, median = calculate_all per_node 0. 0. 0. in
  let result : tpsResponse = {per_node; median; max; min} in
  Dream.json @@ construct_json_to_string tpsResponse_encoding result

let max_and_min_and_average_response max min average =
  Dream.json
  @@ construct_json_to_string
       maxAndMinAndAverageResponse_encoding
       {max; min; average}

let time_to_consensus () =
  let max, min, average =
    MetricsAndState.get_time_to_consensus !metrics_state
  in
  max_and_min_and_average_response max min average

let propagation_times () =
  let max, min, average =
    MetricsAndState.get_propagation_times !metrics_state
  in
  max_and_min_and_average_response max min average

let first_and_last_block_times () =
  let first, last =
    MetricsAndState.get_block_first_and_last_time_blocks !metrics_state
  in
  Dream.json
  @@ construct_json_to_string
       firstAndLastBlockTimesResponse_encoding
       {first; last}

let blocks_per_second () =
  let bps = MetricsAndState.get_blocks_per_second !metrics_state in
  Dream.json @@ construct_json_to_string Data_encoding.float bps

let discarded_blocks () =
  let discarded = MetricsAndState.get_discarded_blocks !metrics_state in
  Dream.json
  @@ construct_json_to_string Data_encoding.int64 (Int64.of_int discarded)
