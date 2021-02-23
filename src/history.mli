open! Core
open Async

(* TODO ayi: we probably want a stable type for our history db *)

type t [@@deriving sexp]

(* TODO ayi: provide functions to incrementally update history instead of always
 * building from scratch *)

(* TODO ayi: provide functions to render log in more human-readable format *)

(* TODO ayi: this function actually raises. Let's use [Deferred.Or_error.t] *)

val create
  :  server:string
  -> start_date:Date.t
  -> end_date:Date.t
  -> cookies_file:Filename.t
  -> now:Time_ns.t
  -> zone:Timezone.t
  -> t Deferred.t
