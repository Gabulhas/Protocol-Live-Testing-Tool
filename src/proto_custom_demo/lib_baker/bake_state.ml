open Protocol
open Alpha_context

let compare_op op1 op2 =
  try Stdlib.compare op1 op2
  with _ ->
    Operation_hash.compare
      (Alpha_context.Operation.hash op1)
      (Alpha_context.Operation.hash op2)

module Operation_set = Set.Make (struct
  type t = operation

  let compare = compare_op
end)

type operation_monitoring_state = {
  mutable operation_pool : Operation_set.t;
  mutable canceler : Lwt_canceler.t;
  lock : Lwt_mutex.t;
}
