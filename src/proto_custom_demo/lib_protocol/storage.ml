open Storage_functors
open Storage_sigs

module Encoding = struct
  module UInt16 : VALUE with type t = int = struct
    type t = int

    let encoding = Data_encoding.uint16
  end

  module Int32 : VALUE with type t = Int32.t = struct
    type t = Int32.t

    let encoding = Data_encoding.int32
  end

  module Int64 : VALUE with type t = Int64.t = struct
    type t = Int64.t

    let encoding = Data_encoding.int64
  end

  module Z : VALUE with type t = Z.t = struct
    type t = Z.t

    let encoding = Data_encoding.z
  end
end

module Int31_index : INDEX with type t = int = struct
  type t = int

  let path_length = 1

  let to_path c l = string_of_int c :: l

  let of_path = function [] | _ :: _ :: _ -> None | [c] -> int_of_string_opt c

  type 'a ipath = 'a * t

  let args =
    Storage_description.One
      {
        rpc_arg = RPC_arg.int;
        encoding = Data_encoding.int31;
        compare = Compare.Int.compare;
      }
end

module Make_index (H : Storage_description.INDEX) :
  INDEX with type t = H.t and type 'a ipath = 'a * H.t = struct
  include H

  type 'a ipath = 'a * t

  let args = Storage_description.One {rpc_arg; encoding; compare}
end

module type Simple_single_data_storage = sig
  type value

  val get : Raw_context.t -> value tzresult Lwt.t

  val update : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t

  val init : Raw_context.t -> value -> Raw_context.t tzresult Lwt.t
end

module Account = struct
  module Raw_context =
    Make_subcontext (Registered) (Raw_context)
      (struct
        let name = ["accounts"]
      end)

  module Global_counter : Simple_single_data_storage with type value = Z.t =
    Make_single_data_storage (Registered) (Raw_context)
      (struct
        let name = ["global_counter"]
      end)
      (Encoding.Z)

  module Indexed_context =
    Make_indexed_subcontext
      (Make_subcontext (Registered) (Raw_context)
         (struct
           let name = ["index"]
         end))
         (Make_index (Account_repr.Index))

  let fold = Indexed_context.fold_keys

  let list = Indexed_context.keys

  module Balance =
    Indexed_context.Make_map
      (struct
        let name = ["balance"]
      end)
      (Tez_repr)

  module Manager =
    Indexed_context.Make_map
      (struct
        let name = ["manager"]
      end)
      (Manager_repr)

  module Counter =
    Indexed_context.Make_map
      (struct
        let name = ["counter"]
      end)
      (Encoding.Z)
end


module Target =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["target"]
    end)
    (Target_repr)

module EpochTime =
  Make_single_data_storage (Registered) (Raw_context)
    (struct
      let name = ["EpochTime"]
    end)
    (Time)

