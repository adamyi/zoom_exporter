open! Core
open Async

(* TODO ayi: we probably want stable types here *)

module Attendee : sig
  type t [@@deriving sexp]
end

type t [@@deriving sexp]

(* TODO ayi: this function actually raises. Let's use [Deferred.Or_error.t] *)

val fetch_t_from_server
  :  server:string
  -> headers:Cohttp.Header.t
  -> meeting_id:string
  -> account_id:string
  -> t Deferred.t
