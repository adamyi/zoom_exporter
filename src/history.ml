open! Core
open Async
open Require_explicit_time_source

type t =
  { last_updated : Time_ns.t
  ; meetings_by_starting_date_till_end_of_month : Meeting.t list Date.Map.t
  }
[@@deriving sexp]

let last_day_of_month date =
  let year = Date.year date in
  let month = Date.month date in
  let day = Date.days_in_month ~year ~month in
  Date.create_exn ~y:year ~m:month ~d:day
;;

let parse_meeting_list_html ~server ~headers html =
  let open Soup.Infix in
  let soup = Soup.parse html in
  let meetings = soup $ "#meeting_list" $ "tbody" $$ "tr" |> Soup.to_list in
  Log.Global.info_s [%message "Found more meetings" ~len:(List.length meetings : int)];
  let idx = ref 0 in
  let has_more =
    let pagination_div = soup $? ".dynamo_pagination" in
    Option.value_map pagination_div ~default:false ~f:(fun pagination_div ->
        pagination_div
        |> Soup.children
        |> Soup.elements
        |> Soup.R.last
        |> Soup.classes
        |> fun classes -> List.mem classes "disabled" ~equal:[%equal: string] |> not)
  in
  if has_more then Log.Global.info_s [%message "List refers to next page"];
  let%map meetings =
    Deferred.List.map meetings ~f:(fun soup ->
        incr idx;
        let modal_link = soup $ "a" in
        let meeting_id = Soup.R.attribute "data-id" modal_link in
        let account_id = Soup.R.attribute "data-accountid" modal_link in
        Log.Global.info_s [%message "Fetching meeting" (!idx : int) meeting_id];
        Meeting.fetch_t_from_server ~server ~headers ~meeting_id ~account_id)
  in
  meetings, has_more
;;

let fetch_meetings_for_single_interval ~server ~start_date ~end_date ~headers =
  Log.Global.info_s
    [%message "Fetching meetings for interval" (start_date : Date.t) (end_date : Date.t)];
  Deferred.repeat_until_finished (1, []) (fun (page, acc) ->
      let%bind rsp, body =
        Cohttp_async.Client.get
          ~headers
          (Uri.of_string
             [%string
               "https://%{server}/account/my/report?p=%{page#Int}&from=%{Date.to_string_american \
                start_date}&to=%{Date.to_string_american end_date}"])
      in
      let%bind meeting_list_html = Cohttp_async.Body.to_string body in
      match rsp |> Cohttp.Response.status |> Cohttp.Code.code_of_status with
      | 200 ->
        let%map meetings, has_more =
          parse_meeting_list_html ~server ~headers meeting_list_html
        in
        let acc = meetings :: acc in
        (match has_more with
        | true -> `Repeat (page + 1, acc)
        | false -> `Finished (List.concat acc))
      | _ ->
        raise_s
          [%message
            "Invalid meeting list response" (rsp : Cohttp.Response.t) meeting_list_html])
;;

let create ~server ~start_date ~end_date ~headers ~now =
  let rec get_interval_list list curr_start_date =
    if Date.(curr_start_date >= end_date)
    then list
    else (
      let curr_end_date = Date.min (last_day_of_month curr_start_date) end_date in
      get_interval_list
        ((curr_start_date, curr_end_date) :: list)
        (Date.add_days curr_end_date 1))
  in
  let intervals = get_interval_list [] start_date in
  let%map meetings_by_starting_date_till_end_of_month =
    Deferred.List.fold
      intervals
      ~init:Date.Map.empty
      ~f:(fun map (start_date, end_date) ->
        let%map data =
          fetch_meetings_for_single_interval ~server ~start_date ~end_date ~headers
        in
        Map.add_exn map ~key:start_date ~data)
  in
  { last_updated = now; meetings_by_starting_date_till_end_of_month }
;;

let create ~server ~start_date ~end_date ~cookies_file ~now ~zone =
  let today = Time_ns.to_date ~zone now in
  if Date.( > ) end_date today
  then
    raise_s
      [%message
        "End date should not be later than today" (today : Date.t) (end_date : Date.t)];
  if Date.(end_date < start_date)
  then
    raise_s
      [%message
        "End date should not be earlier than start date"
          (start_date : Date.t)
          (end_date : Date.t)];
  let%bind cookies = Reader.file_contents cookies_file >>| String.strip ?drop:None in
  let headers = Cohttp.Header.init_with "cookie" cookies in
  create ~server ~start_date ~end_date ~headers ~now
;;
