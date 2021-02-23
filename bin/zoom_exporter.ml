open! Core
open Async

(* TODO ayi: we want more commands to render downloaded sexps to nice formats *)

let command =
  Command.group
    ~summary:"Commands related to exporting and rendering zoom history"
    [ "download-history", Download_history.command ]
;;

let () = Command.run command
