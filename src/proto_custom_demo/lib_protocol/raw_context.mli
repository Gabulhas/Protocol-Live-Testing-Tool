type missing_key_kind = Get | Set | Del | Copy

(** An internal storage error that should not happen *)
type storage_error =
  | Incompatible_protocol_version of string
  | Missing_key of string list * missing_key_kind
  | Existing_key of string list
  | Corrupted_data of string list

type error += Storage_error of storage_error


val storage_error : storage_error -> 'a tzresult

(** {1 Abstract Context} *)

(** Abstract view of the context.
    Includes a handle to the functional key-value database
    ({!Context.t}) along with some in-memory values (gas, etc.). *)
type t

type root = t

val constants : t -> Constants_repr.parametric

val level : t -> int32
val context : t -> Context.t
val timestamp : t -> Time.t





(** Retrieves the state of the database and gives its abstract view.
    It also returns wether this is the first block validated
    with this version of the protocol. *)
val prepare :
  Context.t ->
  level:Int32.t ->
  timestamp:Time.t ->
  t tzresult Lwt.t

type previous_protocol = Genesis of Parameters_repr.t

val prepare_first_block :
  Context.t ->
  level:int32 ->
  timestamp:Time.t ->
  (previous_protocol * t) tzresult Lwt.t

val activate : t -> Protocol_hash.t -> t Lwt.t










type key = string list

type value = bytes

type tree

module type T =
  Raw_context_intf.T
    with type root := root
     and type key := key
     and type value := value
     and type tree := tree

include T with type t := t
