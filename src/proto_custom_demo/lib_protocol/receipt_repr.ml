(*FIXME: Account might be unecessary*)
type balance = Account of Account_repr.t

let balance_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.balance"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Contract"
           (obj2
              (req "kind" (constant "contract"))
              (req "Account" Account_repr.encoding))
           (function Account c -> Some ((), c))
           (fun ((), c) -> Account c);
       ]

type balance_update = Debited of Tez_repr.t | Credited of Tez_repr.t

let balance_update_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.balance_update"
  @@ obj1
       (req
          "change"
          (conv
             (function
               | Credited v -> Tez_repr.to_mutez v
               | Debited v -> Int64.neg (Tez_repr.to_mutez v))
             ( Json.wrap_error @@ fun v ->
               if Compare.Int64.(v < 0L) then
                 match Tez_repr.of_mutez (Int64.neg v) with
                 | Some v -> Debited v
                 | None -> failwith "Qty.of_mutez"
               else
                 match Tez_repr.of_mutez v with
                 | Some v -> Credited v
                 | None -> failwith "Qty.of_mutez" )
             int64))

(*FIXME: Protocol_migration might be unecessary*)
type update_origin = Block_application 

let update_origin_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.update_origin"
  @@ obj1 @@ req "origin"
  @@ union
       [
         case
           (Tag 0)
           ~title:"Block_application"
           (constant "block")
           (function Block_application -> Some () )
           (fun () -> Block_application);
       ]

type balance_updates = (balance * balance_update * update_origin) list

let balance_updates_encoding =
  let open Data_encoding in
  def "operation_metadata.alpha.balance_updates"
  @@ list
       (conv
          (function
            | balance, balance_update, update_origin ->
                ((balance, balance_update), update_origin))
          (fun ((balance, balance_update), update_origin) ->
            (balance, balance_update, update_origin))
          (merge_objs
             (merge_objs balance_encoding balance_update_encoding)
             update_origin_encoding))

let cleanup_balance_updates balance_updates =
  List.filter
    (fun (_, (Credited update | Debited update), _) ->
      not (Tez_repr.equal update Tez_repr.zero))
    balance_updates
