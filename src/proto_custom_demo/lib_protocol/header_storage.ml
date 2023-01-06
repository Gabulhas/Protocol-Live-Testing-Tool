(*EPOCH*)
let init_current_target c =
    Storage.Target.init c Target_repr.zero

let init_current_target_with_target c target=
    Storage.Target.init c target

let get_current_target c = 
    Storage.Target.get c

let update_current_target c target = 
    Storage.Target.update c target


(*EPOCH

Lembrar: Não é preciso pÕr a timestamp do header, visto que o que interessa é saber o tempo que demorou o epoch, não "a" média dos timestamps

 *)
let init_epoch_time c =
    Storage.EpochTime.init c (Time.of_seconds 0L)

let init_epoch_time_with_time c time =
    Storage.EpochTime.init c time

let last_epoch_time c =
    Storage.EpochTime.get c

let update_epoch_time c time =
    Storage.EpochTime.update c time
