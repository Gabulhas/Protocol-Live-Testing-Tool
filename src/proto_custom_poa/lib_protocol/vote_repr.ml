type action = Add | Remove | Ignore [@@deriving encoding {enum}]

type stored_action = {
  action : action;
  endorser : Account_repr.t; [@encoding Account_repr.encoding]
}
[@@deriving encoding]

let empty_stored_action = {action = Ignore; endorser = Account_repr.zero}

type vote = {
  stored_action : stored_action;
  candidate : Account_repr.t; [@encoding Account_repr.encoding]
}
[@@deriving encoding]

type t = vote

let empty = {stored_action = empty_stored_action; candidate = Account_repr.zero}

let encoding = vote_encoding
