open! Core
open! Async

(* TODO ayi: ceebs but write tests *)

let%expect_test _ =
  print_endline "this is a test";
  [%expect {| this is a test |}];
  return ()
;;
