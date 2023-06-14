type contents = {
  target : Target_repr.t;
  nonce : Int64.t;
  miner : Account_repr.t; [@encoding Account_repr.encoding]
}
[@@deriving encoding]

type protocol_data = contents

module Header =
  Custom_protocol_helper.Header_make.MakeHeader
    (struct
      type t = contents

      let encoding = contents_encoding
    end)
    (struct
      type t = contents

      let fake_protocol_data =
        {
          target = Target_repr.zero;
          nonce = 0L;
          miner = Signature.Public_key_hash.zero;
        }

      let encoding = contents_encoding
    end)

include Header
