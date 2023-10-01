type t = {level : int32}

type error += Invalid_fitness of bytes list (* `Permanent *)

let () =
  register_error_kind
    `Permanent
    ~id:"fitness.invalid"
    ~title:"Invalid fitness"
    ~description:"The fitness is invalid"
    ~pp:(fun ppf ll ->
      Format.fprintf
        ppf
        "Invalid fitness: %s"
        (String.concat "::" (List.map Bytes.to_string ll))
    )
    Data_encoding.(
        obj1 
        (req "fitness" (list bytes))
    )
    (function Invalid_fitness blsit -> Some blsit | _ -> None)
    (fun blist -> Invalid_fitness blist)


let encoding =
  let open Data_encoding in
  def
    "fitness"
    (conv
       (fun {level} -> level)
       (fun level -> {level})
       (obj1 (req "level" int32)))

let fitness_from_level level =
  let level = Int64.(of_int32 level) in
  let open Data_encoding in
  [
    Data_encoding.Binary.to_bytes_exn int64 level;
    Bytes.of_string "1";
    Bytes.of_string "\000";
    Bytes.of_string "\000";
    Bytes.of_string "\000";
  ]

let to_raw {level} = fitness_from_level level

let zero = {level = 0l}

let int64_to_bytes i =
  let b = Bytes.make 8 '0' in
  TzEndian.set_int64 b 0 i ;
  b

let int64_of_bytes b =
  if Compare.Int.(Bytes.length b <> 8) then error (Invalid_fitness [b])
  else ok (TzEndian.get_int64 b 0)

let from_int64 fitness = [int64_to_bytes fitness]

let to_int64 = function
  | [version; fitness]
    when Compare.String.(
           Bytes.to_string version = Constants_repr.version_number) ->
      int64_of_bytes fitness
  | [] -> ok 0L
  | _ -> error (Invalid_fitness [])

let level_from_raw raw =
  match raw with
  | hd :: _ -> (
      match int64_of_bytes hd with
      | Ok a -> Int64.to_int32 a |> ok
      | _ -> error (Invalid_fitness raw))
  | _ -> error (Invalid_fitness raw)

let from_raw raw =
  match level_from_raw raw with Ok level -> {level} | _ -> {level = 1l}

let to_string fitness=
    fitness
    |> to_raw
    |> List.map (fun b ->
            match Hex.of_bytes b with
            | `Hex a -> a
    )
    |> String.concat  ","




