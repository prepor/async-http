open! Core.Std
open! Async.Std

module L = Async_http_log

module type Obj =
sig
  module P : sig
    include Hashtbl.Key
    val sexp_of_t : t -> Sexp.t
  end
  type p = P.t
  type t
  val create : P.t -> t Deferred.t
  val is_valid : t -> bool
  val destroy : t -> unit
end

module type S =
sig
  type pobj
  type param
  type pool
  val create : ?expiration_time:Time.Span.t -> unit -> pool
  val checkin : pool -> param -> pobj -> unit
  val checkout : pool -> param -> pobj Deferred.t
end

module Make (Obj : Obj) = struct
  type pobj = Obj.t
  type param = Obj.p

  module Param = struct
    include Hashable.Make (Obj.P)
  end

  type entry = { obj : Obj.t;
                 updated_at : Time.t}
  type pool = { expiration_time : Time.Span.t;
                queues : entry Linked_queue.t Param.Table.t;
                mutable cleaner : bool }

  let create ?(expiration_time = (Time.Span.of_int_sec 1)) () =
    {expiration_time; queues = Param.Table.create (); cleaner = false}

  let rec dequeue q param =
    match Linked_queue.dequeue q with
    | Some v ->
        if Obj.is_valid v.obj then return v.obj
        else (L.debug (fun m -> m "HTTP connection for %s is not valid, forget about it" (Obj.P.sexp_of_t param |> Sexp.to_string_hum));
              dequeue q param)
    | None ->
        L.debug (fun m -> m "HTTP create new conn for %s" (Obj.P.sexp_of_t param |> Sexp.to_string_hum));
        Obj.create param

  let get_queue pool param =
    Param.Table.find_or_add pool.queues param ~default:(fun () -> Linked_queue.create ())

  let checkout pool param =
    let queue = get_queue pool param in
    L.debug (fun m -> m "HTTP checkout conn for %s" (Obj.P.sexp_of_t param |> Sexp.to_string_hum));
    dequeue queue param

  let clean_queue pool q =
    let n = Time.now () in
    let rec w () =
      match Linked_queue.peek q with
      | Some {updated_at; obj} ->
          if (Time.is_later (Time.add updated_at pool.expiration_time) ~than: n)
          then (Linked_queue.dequeue q |> ignore; w ())
          else ()
      | None -> () in
    w ()

  let start_cleaner pool =
    let rec worker () =
      after (Time.Span.of_int_sec 1) >>= fun () ->
      List.iter (Param.Table.keys pool.queues) ~f:(fun k ->
          let q = Param.Table.find_exn pool.queues k in
          clean_queue pool q;
          if Linked_queue.length q = 0 then
            Param.Table.remove pool.queues k);
      if Param.Table.length pool.queues > 0 then worker ()
      else (pool.cleaner <- false;
            return ()) in
    worker () |> don't_wait_for;
    pool.cleaner <- true

  let ensure_cleaner pool =
    if pool.cleaner = false then start_cleaner pool
    else ()

  let checkin pool param obj =
    let queue = get_queue pool param in
    L.debug (fun m -> m "HTTP checkin conn for %s" (Obj.P.sexp_of_t param |> Sexp.to_string_hum));
    Linked_queue.enqueue queue { obj; updated_at = Time.now ()};
    ensure_cleaner pool

end
