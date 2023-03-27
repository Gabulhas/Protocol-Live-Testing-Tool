open Protocol
open Alpha_context

type error += Bad_tez_arg of string * string (* Arg_name * value *)
type error += Invalid_account_notation of string * string
type error += Bad_positive_number_arg of (string * string)

let () =
  register_error_kind
    `Permanent
    ~id:"badTezArg"
    ~title:"Bad Tez Arg"
    ~description:"Invalid \xEA\x9C\xA9 notation in parameter."
    ~pp:(fun ppf (arg_name, literal) ->
      Format.fprintf
        ppf
        "Invalid \xEA\x9C\xA9 notation in parameter %s: '%s'"
        arg_name
        literal)
    Data_encoding.(obj2 (req "parameter" string) (req "literal" string))
    (function
      | Bad_tez_arg (parameter, literal) -> Some (parameter, literal)
      | _ -> None)
    (fun (parameter, literal) -> Bad_tez_arg (parameter, literal)) ;
  register_error_kind
    `Permanent
    ~id:"InvalidAccountNotation"
    ~title:"Invalid Account Notation"
    ~description:"Invalid account notation in parameter."
    ~pp:(fun ppf (arg_name, literal) ->
      Format.fprintf
        ppf
        "Invalid account notation in parameter %s: '%s'"
        arg_name
        literal)
    Data_encoding.(obj2 (req "parameter" string) (req "literal" string))
    (function
      | Invalid_account_notation (parameter, literal) ->
          Some (parameter, literal)
      | _ -> None)
    (fun (parameter, literal) -> Invalid_account_notation (parameter, literal))

let tez_format =
  "Text format: `DDDDDDD.DDDDDD`.\n\
   Tez and mutez and separated by a period sign. Trailing and pending zeroes \
   are allowed."

let tez_parameter param =
  Tezos_clic.parameter (fun _ s ->
      match Tez.of_string s with
      | Some tez -> return tez
      | None -> fail (Bad_tez_arg (param, s)))

let tez_arg ~default ~parameter ~doc =
  Tezos_clic.default_arg
    ~long:parameter
    ~placeholder:"amount"
    ~doc
    ~default
    (tez_parameter ("--" ^ parameter))

let tez_opt_arg ~parameter ~doc =
  Tezos_clic.arg
    ~long:parameter
    ~placeholder:"amount"
    ~doc
    (tez_parameter ("--" ^ parameter))

let tez_param ~name ~desc next =
  Tezos_clic.param
    ~name
    ~desc:(desc ^ " in \xEA\x9C\xA9\n" ^ tez_format)
    (tez_parameter name)
    next

let account_format =
  "Text format: base58check encoded string.\n\
   The account notation is the base58check encoding of the public key hash."

let account_parameter param =
  Tezos_clic.parameter (fun _ s ->
      match Account.of_b58check s with
      | Ok account -> return account
      | Error _ -> fail (Invalid_account_notation (param, s)))

let account_arg ~default ~parameter ~doc =
  Tezos_clic.default_arg
    ~long:parameter
    ~placeholder:"account"
    ~doc
    ~default
    (account_parameter ("--" ^ parameter))

let account_opt_arg ~parameter ~doc =
  Tezos_clic.arg
    ~long:parameter
    ~placeholder:"account"
    ~doc
    (account_parameter ("--" ^ parameter))

let account_param ~name ~desc next =
  Tezos_clic.param
    ~name
    ~desc:(desc ^ " in " ^ account_format)
    (account_parameter name)
    next


let amount_parameter param =
  Tezos_clic.parameter (fun _ s ->
      match Int32.of_string_opt s with
      | Some amount when amount >= 0l -> return amount
      | _ -> fail (Bad_positive_number_arg (param, s)))

let amount_param ~name ~desc next =
  Tezos_clic.param ~name ~desc (amount_parameter name) next
