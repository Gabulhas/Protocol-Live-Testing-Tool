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

