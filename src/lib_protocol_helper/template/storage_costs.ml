let read_access ~path_length ~read_bytes =
  let open Saturation_repr in
  let base_cost = safe_int (200_000 + (5000 * path_length)) in
  Gas_limit_repr.atomic_step_cost
    (add base_cost (mul (safe_int 2) (safe_int read_bytes)))

(* The model for write accesses is the following:

   cost(written_bytes) = 200_000 + 4 * written_bytes
*)
let write_access ~written_bytes =
  let open Saturation_repr in
  Gas_limit_repr.atomic_step_cost
    (add (safe_int 200_000) (mul (safe_int 4) (safe_int written_bytes)))

