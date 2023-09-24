open Responses_and_types.Block

module SharedState (D : sig
  type t

  val initial : t
end) =
struct
  type t = {mutable data : D.t}

  module T = D

  let create () = {data = D.initial}

  let update f state =
    state.data <- f state.data ;
    Lwt.return_unit

  let get state = Lwt.return state.data

  let reset state =
    state.data <- D.initial ;
    Lwt.return_unit
end

module MutexSharedState (D : sig
  type t

  val initial : t
end) =
struct
  module Base = SharedState (D)
  module T = D

  type t = {base : Base.t; mutex : Lwt_mutex.t}

  let create () = {base = Base.create (); mutex = Lwt_mutex.create ()}

  let update f state =
    Lwt_mutex.with_lock state.mutex (fun () -> Base.update f state.base)

  let get state =
    Lwt_mutex.with_lock state.mutex (fun () -> Base.get state.base)

  let reset state =
    Lwt_mutex.with_lock state.mutex (fun () -> Base.reset state.base)
end

module BlockRelated = struct
  type t = {
    num_nodes : int; (*total number of nodes in this test*)
    start_times : (string, int64) Hashtbl.t;
        (*Marks the first time a block was see by any node (just the timestamp)*)
    end_times : (string, int64) Hashtbl.t;
        (*Marks the last time  a block was see by any node *)
    agreements : (string, int list) Hashtbl.t;
        (*List of nodes that agree on a specific block*)
    consensus_times : (string * int64) list ref;
        (*List of pairs of block_id and time taken for consensus, that is, how much time did a block take to reach majority*)
    start_time : int64; (*When the tests started*)
    prev_time : int64 ref;
  }

  let empty () =
    {
      num_nodes = 0;
      start_times = Hashtbl.create 0;
      end_times = Hashtbl.create 0;
      agreements = Hashtbl.create 0;
      consensus_times = ref [];
      start_time = 0L;
      prev_time = ref 0L;
    }

  let create num_nodes first_timestamp =
    {
      num_nodes;
      start_times = Hashtbl.create 10;
      end_times = Hashtbl.create 10;
      agreements = Hashtbl.create 10;
      consensus_times = ref [];
      start_time = first_timestamp;
      prev_time = ref 0L;
    }

  let majority_agree t block =
    let total_nodes = t.num_nodes in
    let required_majority = (total_nodes / 2) + 1 in
    try
      let agreeing_nodes = Hashtbl.find t.agreements block.hash in
      List.length agreeing_nodes >= required_majority
    with Not_found -> false

  let add_block_if_first_time t block =
    let open Tezos_base.Time.Protocol in
    if not (Hashtbl.mem t.start_times block.hash) then
      Hashtbl.add t.start_times block.hash (to_seconds block.header.timestamp)

  let add_block_last_seen t block current_time =
    Hashtbl.replace t.end_times block.hash current_time

  let add_to_aggreements t block node_id =
    match Hashtbl.find_opt t.agreements block.hash with
    | None -> Hashtbl.add t.agreements block.hash [node_id]
    | Some already_added ->
        Hashtbl.replace t.agreements block.hash (node_id :: already_added)

  let check_reached_consensus t block current_time =
    if majority_agree t block then
      let end_time = current_time in
      let start_time = Hashtbl.find t.start_times block.hash in
      let time_to_consensus = Int64.sub end_time start_time in
      t.consensus_times :=
        (block.hash, time_to_consensus) :: !(t.consensus_times)

  let new_block t block node_id current_time =
    add_block_if_first_time t block ;
    add_block_last_seen t block current_time ;
    add_to_aggreements t block node_id ;
    check_reached_consensus t block current_time

  (*Returns max, min and avg of time for a block to be accepted by majority*)
  let get_time_to_consensus t =
    let rec calc consensus_times maximum minimum total n_consensus =
      match consensus_times with
      | [] ->
          ( maximum,
            minimum,
            if List.length !(t.consensus_times) > 0 then
              Int64.to_float total /. float_of_int n_consensus
            else 0. )
      | (_block_id, conesensus_time) :: tl ->
          calc
            tl
            (Int64.max maximum conesensus_time)
            (Int64.min minimum conesensus_time)
            (Int64.add total conesensus_time)
            (succ n_consensus)
    in
    calc !(t.consensus_times) 0L Int64.max_int 0L 0

  let get_block_first_and_last_time_blocks t =
    let last_consensus_block, _ = List.hd !(t.consensus_times) in
    let first_consensus_block, _ = List.hd @@ List.rev !(t.consensus_times) in
    let first_block_time = Hashtbl.find t.start_times first_consensus_block in
    (*Change this so it's actually when the block was official in the chain*)
    let last_block_time = Hashtbl.find t.start_times last_consensus_block in
    (first_block_time, last_block_time)

  let get_propagation_times t =
    let block_propagation_time block_hash =
      let first_seen = Hashtbl.find t.start_times block_hash in
      let last_seen = Hashtbl.find t.end_times block_hash in
      Int64.sub last_seen first_seen
    in
    let maximum, minimum, total =
      List.fold_left
        (fun (maximum, minimum, total) (block_id, _) ->
          let propation_time = block_propagation_time block_id in
          ( Int64.max propation_time maximum,
            Int64.min propation_time minimum,
            Int64.add propation_time total ))
        (-1L, Int64.max_int, 0L)
        !(t.consensus_times)
    in

    ( maximum,
      minimum,
      Int64.to_float total /. float_of_int (List.length !(t.consensus_times)) )

  let get_blocks_per_second t =
    match !(t.consensus_times) with
    | [] -> -1.
    | _ ->
        let first_time, last_time = get_block_first_and_last_time_blocks t in
        let number_of_blocks = List.length !(t.consensus_times) in
        float_of_int number_of_blocks
        /. (Int64.to_float @@ Int64.sub last_time first_time)

  let get_discarded_blocks t =
    let all_blocks_number = Hashtbl.length t.start_times in
    let reached_consensus = List.length !(t.consensus_times) in
    all_blocks_number - reached_consensus
end

module MetricsAndState = struct
  type t = {tps : float array; block_related : BlockRelated.t}

  let create num_nodes first_timestamp =
    let tps = Array.make num_nodes 0.0 in
    Lwt.return
      {tps; block_related = BlockRelated.create num_nodes first_timestamp}

  let empty () = {tps = Array.make 0 0.; block_related = BlockRelated.empty ()}

  let new_block t block node_id current_time =
    BlockRelated.new_block t.block_related block node_id current_time

  let get_time_to_consensus t =
    BlockRelated.get_time_to_consensus t.block_related

  let get_block_first_and_last_time_blocks t =
    BlockRelated.get_block_first_and_last_time_blocks t.block_related

  let get_propagation_times t =
    BlockRelated.get_propagation_times t.block_related

  let get_blocks_per_second t =
    BlockRelated.get_blocks_per_second t.block_related

  let get_discarded_blocks t = BlockRelated.get_discarded_blocks t.block_related
end

let metrics_state = ref (MetricsAndState.empty ())
