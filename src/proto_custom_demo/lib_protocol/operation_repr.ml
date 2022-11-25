type raw = Operation.t = {shell : Operation.shell_header; proto : bytes}

let raw_encoding = Operation.encoding

type 'kind operation = {
  shell : Operation.shell_header;
  protocol_data : 'kind protocol_data;
}

and 'kind protocol_data = {
  contents : 'kind contents_list;
  signature : Signature.t option;
}

and _ contents_list =
  | Single : 'kind contents -> 'kind contents_list
  | Cons :
      'kind Kind.manager contents * 'rest Kind.manager contents_list
      -> ('kind * 'rest) Kind.manager contents_list

and _ contents =
  | Preendorsement : consensus_content -> Kind.preendorsement contents
  | Endorsement : consensus_content -> Kind.endorsement contents
  | Dal_slot_availability :
      Signature.Public_key_hash.t * Dal_endorsement_repr.t
      -> Kind.dal_slot_availability contents

