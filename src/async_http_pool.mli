open! Core.Std
open! Async.Std

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

module Make (Obj : Obj) : S with type pobj = Obj.t and type param = Obj.p
