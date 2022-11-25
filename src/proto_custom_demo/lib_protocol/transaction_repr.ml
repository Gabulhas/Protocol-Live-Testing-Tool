type transaction = {
    amount: Tez_repr.tez;
    destination : Signature.Public_key_hash.t;
}

type error =
   Negative_value_transfer of Tez_repr.t(* `Temporary *)

let create amount destination =
    let open Tez_repr in
    if amount < Tez_repr.zero then 
        error (Negative_value_transfer amount)
    else
        ok ({amount; destination})

let check_signature trans =
    
    

type t = transaction

open Data_encoding

let encoding: transaction encoding =
    def "operation.transaction"
    @@ conv
           (fun {amount; destination} -> (amount, destination))
           (fun (amount, destination) -> {amount; destination})
           (merge_objs Tez_repr.encoding Signature.Public_key_hash.encoding)

