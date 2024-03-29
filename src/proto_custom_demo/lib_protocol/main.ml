type block_header_data = Alpha_context.Block_header.protocol_data

type block_header = Alpha_context.Block_header.t = {
  shell : Block_header.shell_header;
  protocol_data : block_header_data;
}


let block_header_data_encoding = Alpha_context.Block_header.protocol_data_encoding

let max_operation_data_length = 32 * 1024 (*Move this to constants*)

let max_block_length = Alpha_context.Block_header.max_header_length

let validation_passes = Updater.[{max_size = 1000000; max_op = None }]

let acceptable_pass _op = Some 0

type block_header_metadata = Apply_results.block_metadata
let block_header_metadata_encoding =  Apply_results.block_metadata_encoding

(** The economic protocol-specific type of operations. *)
(*OPeration data is the same data as the operation itself*)
type operation_data = Alpha_context.Operation.protocol_data

(** Result o applying an operation *)
type operation_receipt = Apply_results.operation_result

type operation = Alpha_context.operation = {
  shell : Operation.shell_header;
  protocol_data : operation_data;
}

let operation_receipt_encoding = Apply_results.operation_result_encoding

let operation_data_encoding = Operation_repr.protocol_data_encoding

let operation_data_and_receipt_encoding =
  (* we could merge data and receipt encoding for a lighter json *)
  Data_encoding.(
    obj2 (req "data" operation_data_encoding) (req "receipt" operation_receipt_encoding))

let acceptable_passes _op = [0]

let relative_position_within_block (a: operation) (b: operation): int = 
    match a.protocol_data.content, b.protocol_data.content with
    | Management a, Management b -> (
        if Account_repr.equal a.source b.source then
            Z.compare a.counter b.counter
        else
            0
    )



type validation_mode =
  | Application of {
      block_header : Alpha_context.Block_header.t;
      fitness: Alpha_context.Fitness.t;
    }
  | Partial_application of {
      block_header : Alpha_context.Block_header.t;
      fitness: Alpha_context.Fitness.t;
    }
  | Partial_construction of {
      predecessor : Block_hash.t;
      predecessor_fitness: Fitness.t;
      predecessor_level : Int32.t;
  }
  | Full_construction of {
      predecessor : Block_hash.t;
      miner : Account_repr.t;
      level : Int32.t;
      protocol_data : Alpha_context.Block_header.contents;
      predecessor_level : Int32.t;
    }

type validation_state = {
  ctxt : Alpha_context.t;
  op_count : int;
  mode: validation_mode;
}


let begin_application ~chain_id:_ ~predecessor_context ~predecessor_timestamp ~predecessor_fitness:_ (block_header: block_header) =
    let level = block_header.shell.level in
        let ctxt = predecessor_context in
        let timestamp =block_header.shell.timestamp  in 
        Logging.log Notice "begin_application: level %s, timestamp %s" (Int32.to_string level) (Time.to_notation timestamp);
        Alpha_context.prepare ctxt ~level ~timestamp:predecessor_timestamp >>=? fun ctxt ->
        Apply.begin_application ctxt block_header timestamp >|=? fun ctxt ->

        let fitness= Fitness_repr.({level}) in
        let mode =
            Application {block_header; fitness} in
        {ctxt; op_count=0; mode}


let begin_partial_application ~chain_id:_ ~ancestor_context ~predecessor_timestamp ~predecessor_fitness:_ (block_header: block_header) =
    let level = block_header.shell.level in
        let ctxt = ancestor_context in
        let timestamp =block_header.shell.timestamp  in 
        let predecessor_level = Int32.pred level in
        Logging.log Notice "begin_partial_application: level %s, timestamp %s" (Int32.to_string level) (Time.to_notation timestamp);
        Alpha_context.prepare ctxt ~level:predecessor_level ~timestamp:predecessor_timestamp >>=? fun ctxt ->
        Apply.begin_partial_application ctxt block_header >|=? fun ctxt ->
        let mode =
            Partial_application {block_header; fitness= Fitness_repr.{level=predecessor_level}} in
        {ctxt; op_count=0; mode}

let begin_construction ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp:_ ~predecessor_level ~predecessor_fitness ~predecessor ~timestamp ?(protocol_data : block_header_data option) () =
  let level = Int32.succ predecessor_level in
  Logging.log Notice "begin_construction %s | predecessor_level %s | predecessor_fitness %s | predecessor_hash %s | protocol_data %s" 
  (match protocol_data with Some _ -> "Full construction" | _ -> "Partial construction")
  (Int32.to_string predecessor_level)
  (Utils.to_string_json Fitness.encoding predecessor_fitness)
  (predecessor |> Block_hash.to_bytes |> Hex.of_bytes |> function `Hex a -> a)
  (match protocol_data with None -> "" | Some a -> Utils.to_string_json block_header_data_encoding a)
  ;

  Alpha_context.prepare ctxt ~level ~timestamp >>=? fun ctxt ->
  ( match protocol_data with
  | None ->

      let mode = Partial_construction {predecessor; predecessor_fitness; predecessor_level} in
      Apply.calculate_current_target ctxt (Raw_context.level ctxt) timestamp >>=? fun (current_target, ctxt) ->
      Logging.log Notice "PARTIAL_CONSTRUCTION CURRENT TARGET %s" (current_target |> Target_repr.to_hex_string);

      Lwt.return (ok(mode, ctxt))

  | Some protocol_data ->
      let mode =
          Full_construction {predecessor; level; protocol_data; predecessor_level; miner=protocol_data.miner}
      in
      Apply.begin_construction ctxt timestamp protocol_data >|=? fun ctxt ->
      (mode, ctxt)
  )
  >|=? fun (mode, ctxt) ->
  {mode; ctxt; op_count = 0}



let apply_operation ({ctxt; op_count; _} as data) (operation: operation)  =
  Logging.log Notice "apply_operation %s" (Utils.to_string_json Operation_repr.encoding operation);
  Apply.apply_operation ctxt
      operation
    >|=? fun (ctxt, result) ->
        let op_count = op_count + 1 in

        ({data with ctxt; op_count}, result)


(**)

let cache_nonce_from_block_header shell contents =
    let open Alpha_context.Block_header in
    let shell =
        Block_header.
      {
        level = Int32.of_int 0;
        proto_level = 0;
        predecessor = shell.predecessor;
        timestamp = Time.of_seconds 0L;
        validation_passes = 0;
        operations_hash = shell.operations_hash;
        fitness = [];
        context = Context_hash.zero;
          }
    in
  let contents =
      {
          contents with
      nonce = Int64.zero;
      miner = Signature.Public_key_hash.zero;
      }
    in
  let protocol_data = contents in
  let x = {shell; protocol_data} in
  Block_hash.to_bytes (hash x)

type error += Missing_shell_header

let () =
    register_error_kind
    `Permanent
    ~id:"main.missing_shell_header"
    ~title:"Missing shell_header during finalisation of a block"
    ~description:
        "During finalisation of a block header in Application mode or Full \
       construction mode, a shell header should be provided so that a cache \
       nonce can be computed."
    ~pp:(fun ppf () ->
        Format.fprintf
        ppf
        "No shell header provided during the finalisation of a block.")
    Data_encoding.unit
    (function Missing_shell_header -> Some () | _ -> None)
    (fun () -> Missing_shell_header)




let finalize_block {mode; ctxt; op_count} (shell_header: Block_header.shell_header option) : (Updater.validation_result * block_header_metadata, error trace) result Lwt.t=
    Logging.log Notice "finalize_block %s"
  (Raw_context.to_string ctxt)
  ;
  match mode with
  | Partial_construction {predecessor_fitness; _} ->
          let level = Alpha_context.level ctxt in

          Logging.log Notice "Finalize Partial_construction: Fitness %s | Level %s | Shell_header %s  | ctxt level: %s" 
          (predecessor_fitness |> Fitness.to_bytes |> Hex.of_bytes |>  function `Hex a -> a) 
          (Int32.to_string level)
          (match shell_header with None -> "None" | Some a -> Utils.to_string_json (Block_header.shell_header_encoding) a)
          (ctxt |> Raw_context.level |> Int32.to_string)
          ;

          let fitness = predecessor_fitness in
          let block_timestamp = Alpha_context.timestamp ctxt in
          let ctxt = Alpha_context.finalize ctxt fitness in
          ( ctxt, Apply_results.{ 
              level;
          block_timestamp;
  }) |> ok |> Lwt.return

  | Partial_application {fitness; _} ->
          let level = Alpha_context.level ctxt in

          Logging.log Notice "Finalize Partial_application: Fitness %s | Level %s | Shell_header %s  | ctxt level: %s" 
          (Fitness_repr.to_string fitness)
          (Int32.to_string level)
          (match shell_header with None -> "None" | Some a -> Utils.to_string_json (Block_header.shell_header_encoding) a)
          (ctxt |> Raw_context.level |> Int32.to_string)
          ;

          let block_timestamp = Alpha_context.timestamp ctxt in
          let ctxt = Alpha_context.finalize ctxt (Fitness_repr.to_raw fitness) in
          ( ctxt, Apply_results.{ 
              level;
          block_timestamp;
  }) |> ok |> Lwt.return


  | Application {block_header = {protocol_data; _}; _}
  | Full_construction {protocol_data; _} ->
          let application_or_construction = match mode with 
            | Application _ -> "Application"
            | Full_construction _ -> "Full Construction"
            | _ -> assert false
          in
          (*Check the target and stuff*)
          Option.value_e shell_header ~error:(Error_monad.trace_of_error Missing_shell_header) >>?= fun shell_header ->

              let cache_nonce =
                  cache_nonce_from_block_header shell_header protocol_data
          in
      let level = shell_header.level in
      Alpha_context.Cache.Admin.sync ctxt ~cache_nonce >>= fun ctxt ->
          let fitness = Fitness_repr.to_raw {level} in
          let block_timestamp = Alpha_context.timestamp ctxt in
          Logging.log Notice "Finalize %s: Fitness Level %s | Shell_header %s  | ctxt level: %s" 
          (application_or_construction)
          (Int32.to_string level)
          (Utils.to_string_json (Block_header.shell_header_encoding) shell_header)
          (ctxt |> Raw_context.level |> Int32.to_string)
          ;

          let commit_message =
              Format.asprintf
          "lvl %ld, %d ops"
          level
          op_count

          in
      let ctxt = Alpha_context.finalize ~commit_message ctxt fitness in
      ( ctxt, Apply_results.{ 
          level;
          block_timestamp;

          }) |> ok |> Lwt.return




let init chain_id ctxt block_header =
    Logging.log Notice "init: Initializing Protocol";
    let level = block_header.Block_header.level in
    let timestamp = block_header.timestamp in
    Logging.log Notice "init: ContextHash %s | Level %s | Timestamp %s | Chain_id %s" 
    (block_header.context |> Context_hash.to_bytes |> Utils.bytes_to_hex_string )
    (Int32.to_string level) 
    (Time.to_notation timestamp)
    ( chain_id |> Chain_id.to_bytes |> Utils.bytes_to_hex_string)
    ;

  Alpha_context.prepare_first_block ctxt ~level ~timestamp  >>=? fun ctxt ->  
      let cache_nonce =
          cache_nonce_from_block_header
      block_header
      {
          target= Z.zero;
        nonce= Int64.zero;
        miner = Signature.Public_key_hash.zero;
      }
  in
  let fitness = Fitness_repr.({level}) in
  let fitness_raw = Fitness_repr.to_raw fitness in
  Alpha_context.Cache.Admin.sync ctxt ~cache_nonce >>= fun ctxt ->
      return (Alpha_context.finalize ctxt fitness_raw)


          let rpc_services =
              Services.register ();
  Services_registration.get_rpc_services ()

          let compare_operations _ _ = 0

          let value_of_key ~chain_id:_ ~predecessor_context:ctxt ~predecessor_timestamp:_
    ~predecessor_level:pred_level ~predecessor_fitness:_ ~predecessor:_
    ~timestamp =
        let level = Int32.succ pred_level in
        Alpha_context.prepare ctxt ~level ~timestamp
  >>=? fun ctxt -> return (Apply.value_of_key ctxt)
