type index = int

type size = int

type identifier = string

type namespace = string

let compare_namespace = Compare.String.compare

type internal_identifier = {namespace : namespace; id : identifier}

let separator = '@'

let sanitize namespace =
  if String.contains namespace separator then
    invalid_arg
      (Format.asprintf
         "Invalid cache namespace: '%s'. Character %c is forbidden."
         namespace
         separator)
  else namespace

let create_namespace = sanitize

let string_of_internal_identifier {namespace; id} =
  namespace ^ String.make 1 separator ^ id

let internal_identifier_of_string raw =
  match String.index_opt raw separator with
  | None -> assert false
  | Some index ->
      {
        (* We do not need to call sanitize here since we stop at the first '@'
            from index 0. It is a guarantee that there is no '@' between 0 and
           (index - 1 ). *)
        namespace = String.sub raw 0 index;
        id =
          (let delim_idx = index + 1 in
           String.sub raw delim_idx (String.length raw - delim_idx));
      }

let internal_identifier_of_key key =
  let raw = Raw_context.Cache.identifier_of_key key in
  internal_identifier_of_string raw

let key_of_internal_identifier ~cache_index identifier =
  let raw = string_of_internal_identifier identifier in
  Raw_context.Cache.key_of_identifier ~cache_index raw

let make_key =
  let namespaces = ref [] in
  fun ~cache_index ~namespace ->
    if List.mem ~equal:String.equal namespace !namespaces then
      invalid_arg
        (Format.sprintf "Cache key namespace %s already exist." namespace)
    else (
      namespaces := namespace :: !namespaces ;
      fun ~id ->
        let identifier = {namespace; id} in
        key_of_internal_identifier ~cache_index identifier)

module NamespaceMap = Map.Make (struct
  type t = namespace

  let compare = compare_namespace
end)

type partial_key_handler =
  Raw_context.t -> string -> Context.Cache.value tzresult Lwt.t

let value_of_key_handlers : partial_key_handler NamespaceMap.t ref =
  ref NamespaceMap.empty



module Admin = struct
  include Raw_context.Cache

  let future_cache_expectation ?blocks_before_activation:_ ctxt ~time_in_blocks:_ =
        return @@ Raw_context.Cache.clear ctxt

  let list_keys context ~cache_index =
    Raw_context.Cache.list_keys context ~cache_index

  let key_rank context key = Raw_context.Cache.key_rank context key

  let value_of_key ctxt key =
    (* [value_of_key] is a maintenance operation: it is typically run
       when a node reboots. For this reason, this operation is not
       carbonated. *)
    let {namespace; id} = internal_identifier_of_key key in
    match NamespaceMap.find namespace !value_of_key_handlers with
    | Some value_of_key -> value_of_key ctxt id
    | None ->
        failwith
          (Format.sprintf "No handler for key `%s%c%s'" namespace separator id)
end


