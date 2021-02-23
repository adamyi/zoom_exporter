open! Core
open Async
open Require_explicit_time_source

(** Raw JSON format Zoom server is using. *)
module Raw = struct
  module Attendee = struct
    type t =
      { id : string
      ; name : string
      ; newName : string option
      ; join : int
      ; duration : int
      ; email : string option
      }
    [@@deriving sexp_of, of_yojson] [@@yojson.allow_extra_fields]
  end

  module MeetingReport = struct
    type t =
      { id : string
      ; number : int
      ; topic : string
      ; startTime : int
      ; endTime : int
      }
    [@@deriving sexp_of, of_yojson] [@@yojson.allow_extra_fields]
  end

  type t =
    { attendees : Attendee.t list
    ; meetingReport : MeetingReport.t
    }
  [@@deriving sexp_of, of_yojson] [@@yojson.allow_extra_fields]
end

module Attendee = struct
  type t =
    { name : string
    ; newName : string option
    ; email : string option
    ; intervals : (Time_ns.t * Time_ns.Span.t) list
    ; totalDuration : Time_ns.Span.t
    }
  [@@deriving sexp]

  let merge_raw ~raw:({ id = _; name; newName; join; duration; email } : Raw.Attendee.t) t
    =
    let join = join |> Time_ns.Span.of_int_sec |> Time_ns.of_span_since_epoch in
    let duration = duration |> Time_ns.Span.of_int_sec in
    let interval = join, duration in
    match t with
    | None -> { name; newName; email; intervals = [ interval ]; totalDuration = duration }
    | Some t ->
      { t with
        intervals = interval :: t.intervals
      ; totalDuration = Time_ns.Span.( + ) duration t.totalDuration
      }
  ;;
end

type t =
  { id : string
  ; number : int
  ; topic : string
  ; startTime : Time_ns.t
  ; endTime : Time_ns.t
  ; attendees : Attendee.t String.Map.t
  }
[@@deriving sexp]

let of_raw ({ attendees; meetingReport } : Raw.t) =
  let attendees =
    List.fold attendees ~init:String.Map.empty ~f:(fun map raw_attendee ->
        Map.update map raw_attendee.id ~f:(fun attendee ->
            Attendee.merge_raw ~raw:raw_attendee attendee))
  in
  let startTime =
    meetingReport.startTime / 1000
    |> Time_ns.Span.of_int_sec
    |> Time_ns.of_span_since_epoch
  in
  let endTime =
    meetingReport.endTime / 1000 |> Time_ns.Span.of_int_sec |> Time_ns.of_span_since_epoch
  in
  { id = meetingReport.id
  ; topic = meetingReport.topic
  ; number = meetingReport.number
  ; startTime
  ; endTime
  ; attendees
  }
;;

let fetch_t_from_server ~server ~headers ~meeting_id ~account_id =
  let query = [ "meetingId", meeting_id; "accountId", account_id ] in
  let uri =
    [%string "https://%{server}/account/my/report/participants/list"]
    |> Uri.of_string
    |> (Fn.flip Uri.with_query') query
  in
  let%bind rsp, body = Cohttp_async.Client.get ~headers uri in
  let%bind meeting_json = Cohttp_async.Body.to_string body in
  match rsp |> Cohttp.Response.status |> Cohttp.Code.code_of_status with
  | 200 -> Yojson.Safe.from_string meeting_json |> [%of_yojson: Raw.t] |> of_raw |> return
  | _ ->
    raise_s
      [%message "Invalid meeting json response" (rsp : Cohttp.Response.t) meeting_json]
;;
