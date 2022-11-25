type t = {
  level : int64;
}

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
