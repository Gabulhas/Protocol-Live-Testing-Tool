(*
Here you should define protocol part of the block header
Only update the "contents" type and fake_protocol_data parts

 *)
type contents = {  } [@@deriving encoding]

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
            (*This should be the same type as contents, but defined with fake/zero valueThis should be the same type as contents, but defined with fake/zero valuess*)
        }

      let encoding = contents_encoding
    end)

include Header

