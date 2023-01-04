type t = {
  level : int64;
}

type error += Invalid_fitness (* `Permanent *)

let encoding =
  let open Data_encoding in
  def
    "fitness"
    (conv
       (fun {level} ->
           level)
       (fun (level) -> {level})
       (obj1
          (req "level" int64)
          )
       )

let to_raw {level} = 
    let open Data_encoding in
    [Data_encoding.Binary.to_bytes_exn int64 level]

let zero =
    {level= 0L}

    let int64_to_bytes i =
  let b = Bytes.make 8 '0' in
  TzEndian.set_int64 b 0 i ; b

let int64_of_bytes b =
  if Compare.Int.(Bytes.length b <> 8) then error Invalid_fitness
  else ok (TzEndian.get_int64 b 0)

let from_int64 fitness =
  [int64_to_bytes fitness]

(*
let to_int64 = function
  | [version; fitness]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.version_number) ->
      int64_of_bytes fitness
  | [version; _fitness (* ignored since higher version takes priority *)]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.version_number_004) ->
      ok 0L
  | [] ->
      ok 0L
  | _ ->
      error Invalid_fitness
*)
