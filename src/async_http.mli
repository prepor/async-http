(** Simple HTTP client and server for async

    {e %%VERSION%% — {{:%%PKG_HOMEPAGE%% }homepage}} *)

(** {1 Async-http} *)

open! Core.Std
open! Async.Std

exception ProtocolError of string
exception AddrError of string * string

module Response : sig
  type _ body = String : string body
              | Parsed : (string -> 'a) -> 'a body
  type 'a t = { status : int;
                version : string;
                headers : (string * string) list;
                body : 'a }
end

module Blueprint : sig
  type ('a, 'b) t constraint 'a = [< `With_body | `Without_body]
end

type addr = [`Unix of string | `Inet of (string * int)]

val request_of_addr : addr -> ([< `Without_body], string) Blueprint.t

val request_of_uri : Uri.t -> ([< `Without_body], string) Blueprint.t

val request : string -> ([< `Without_body], string) Blueprint.t

val path : string -> ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val parser : (string -> 'c) -> ('a, 'b) Blueprint.t -> ('a, 'c) Blueprint.t

val query_param : string -> string -> ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val query_params : (string * string) list -> ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val header : string -> string -> ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val headers : (string * string) list -> ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val body : string -> ([< `Without_body], 'b) Blueprint.t -> ([< `With_body], 'b) Blueprint.t

val ssl : ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val not_persistent : ('a, 'b) Blueprint.t -> ('a, 'b) Blueprint.t

val get : ([< `Without_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
val post : ([< `With_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
val put : ([< `With_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
val delete : ([< `Without_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
val options : ([< `Without_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
val head : ([< `Without_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
val patch : ([< `With_body], 'b) Blueprint.t -> ('b Response.t, exn) Deferred.Result.t
