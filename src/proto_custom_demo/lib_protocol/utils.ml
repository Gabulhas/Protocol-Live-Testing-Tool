let to_string_json encoding value_to_encode =
  Format.asprintf
    "%a"
    Data_encoding.Json.pp
    (Data_encoding.Json.construct encoding value_to_encode)

let integer_positive_pow a b : int =
  let open Compare.Int in
  let rec aux c res = if c > 0 then aux (pred c) (res * a) else res in

  aux b 1
