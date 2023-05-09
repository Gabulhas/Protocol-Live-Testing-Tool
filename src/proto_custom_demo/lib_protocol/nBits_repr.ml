(*So far is a copy of Tez_repr*)
module Name = struct
  let name = "custom-demo"
end

module A = Tezos_protocol_environment.V6.Make (Name) ()

module B = Custom_protocol_helper.Header_make.MakeHeader (A)

let id = "nbits"

let name = "nBits"

open Compare.Int64 (* invariant: positive *)

type repr = t

type t = NBit_tag of repr [@@ocaml.unboxed]

type error +=
  | Addition_overflow of t * t (* `Temporary *)
  | Subtraction_underflow of t * t (* `Temporary *)
  | Multiplication_overflow of t * int64 (* `Temporary *)
  | Negative_multiplicator of t * int64 (* `Temporary *)
  | Invalid_divisor of t * int64

(* `Temporary *)

let zero = NBit_tag 0L

let mul_int (NBit_tag nbits) i = NBit_tag (Int64.mul nbits i)

let ( -? ) nbits1 nbits2 =
  let (NBit_tag t1) = nbits1 in
  let (NBit_tag t2) = nbits2 in
  if t2 <= t1 then ok (NBit_tag (Int64.sub t1 t2))
  else error (Subtraction_underflow (nbits1, nbits2))

let sub_opt (NBit_tag t1) (NBit_tag t2) =
  if t2 <= t1 then Some (NBit_tag (Int64.sub t1 t2)) else None

let ( +? ) nbits1 nbits2 =
  let (NBit_tag t1) = nbits1 in
  let (NBit_tag t2) = nbits2 in
  let t = Int64.add t1 t2 in
  if t < t1 then error (Addition_overflow (nbits1, nbits2)) else ok (NBit_tag t)

let ( *? ) nbits m =
  let (NBit_tag t) = nbits in
  if m < 0L then error (Negative_multiplicator (nbits, m))
  else if m = 0L then ok (NBit_tag 0L)
  else if t > Int64.(div max_int m) then
    error (Multiplication_overflow (nbits, m))
  else ok (NBit_tag (Int64.mul t m))

let ( /? ) nbits d =
  let (NBit_tag t) = nbits in
  if d <= 0L then error (Invalid_divisor (nbits, d))
  else ok (NBit_tag (Int64.div t d))

let mul_exn t m =
  match t *? Int64.(of_int m) with
  | Ok v -> v
  | Error _ -> invalid_arg "mul_exn"

let div_exn t d =
  match t /? Int64.(of_int d) with
  | Ok v -> v
  | Error _ -> invalid_arg "div_exn"

let encoding =
  let open Data_encoding in
  let decode (NBit_tag t) = Z.of_int64 t in
  let encode = Json.wrap_error (fun i -> NBit_tag (Z.to_int64 i)) in
  Data_encoding.def name (check_size 10 (conv decode encode n))

type nBits = t

let compare (NBit_tag x) (NBit_tag y) = compare x y

let ( = ) (NBit_tag x) (NBit_tag y) = x = y

let ( <> ) (NBit_tag x) (NBit_tag y) = x <> y

let ( < ) (NBit_tag x) (NBit_tag y) = x < y

let ( > ) (NBit_tag x) (NBit_tag y) = x > y

let ( <= ) (NBit_tag x) (NBit_tag y) = x <= y

let ( >= ) (NBit_tag x) (NBit_tag y) = x >= y

let equal (NBit_tag x) (NBit_tag y) = equal x y

let max (NBit_tag x) (NBit_tag y) = NBit_tag (max x y)

let min (NBit_tag x) (NBit_tag y) = NBit_tag (min x y)
