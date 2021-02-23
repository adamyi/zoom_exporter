open! Core
open Async

let time_source = Time_source.wall_clock ()
let zone = force Timezone.local
let start_time = Time_source.now time_source

let command =
  Command.async
    ~summary:"Print zoom history as a sexp"
    (let%map_open.Command start_date =
       flag "start-date" (required date) ~doc:"DATE Start date"
     and end_date =
       flag_optional_with_default_doc
         "end-date"
         date
         [%sexp_of: Date.t]
         ~default:(Time_ns.to_date ~zone start_time)
         ~doc:"DATE End date"
     and server =
       flag_optional_with_default_doc
         "zoom-domain"
         string
         [%sexp_of: string]
         ~default:"zoom.us"
         ~doc:"STRING Zoom server to connect to"
     and cookies_file =
       flag_optional_with_default_doc
         "cookies-file"
         Filename.arg_type
         [%sexp_of: Filename.t]
         ~default:".cookies"
         ~doc:"FILENAME Read cookies from file"
     in
     fun () ->
       let%bind history =
         Zoom_exporter_lib.History.create
           ~server
           ~start_date
           ~end_date
           ~cookies_file
           ~now:start_time
           ~zone
       in
       print_s ([%sexp_of: Zoom_exporter_lib.History.t] history);
       Writer.flushed (force Writer.stdout))
;;
