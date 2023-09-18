module SharedState (D : sig
  type t

  val initial : t
end) =
struct
  type t = {mutable data : D.t}

  module T = D

  let create () = {data = D.initial}

  let update f state =
    state.data <- f state.data ;
    Lwt.return_unit

  let get state = Lwt.return state.data

  let reset state =
    state.data <- D.initial ;
    Lwt.return_unit
end

module MutexSharedState (D : sig
  type t

  val initial : t
end) =
struct
  module Base = SharedState (D)
  module T = D

  type t = {base : Base.t; mutex : Lwt_mutex.t}

  let create () = {base = Base.create (); mutex = Lwt_mutex.create ()}

  let update f state =
    Lwt_mutex.with_lock state.mutex (fun () -> Base.update f state.base)

  let get state =
    Lwt_mutex.with_lock state.mutex (fun () -> Base.get state.base)

  let reset state =
    Lwt_mutex.with_lock state.mutex (fun () -> Base.reset state.base)
end

module MetricsAndState = struct
  type t = {tps : float array}

  let create num_nodes =
    let tps = Array.make num_nodes 0.0 in
    Lwt.return {tps}
end
