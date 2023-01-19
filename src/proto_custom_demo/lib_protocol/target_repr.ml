type t = Z.t


let zero = Z.of_string "0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"

let encoding = Data_encoding.z

let to_hex d= 
    let z_as_hex_string = Z.format "%x" d in
    let open Compare.Int in
    if String.length z_as_hex_string > 64 then
        raise (Failure "Z is out of range")
    else
        `Hex ( String.make (64 - String.length z_as_hex_string) '0' ^ z_as_hex_string)

let to_bytes d =
    d |> to_hex |> Hex.to_bytes


let adjust (target: t) (time_difference_ratio:int64) =
    (*
    time_difference_ratio = time_taken this epoch/ block_time * epoch_size
     *)
    Z.mul target (Z.of_int64 time_difference_ratio)

let to_hex_string d = 
    let as_hex = to_hex d  in
    match as_hex with 
    | `Hex a -> a

