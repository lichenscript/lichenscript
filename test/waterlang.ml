
open OUnit2
open Waterlang_parsing

let test_parser _ =
  let _ =  Parser.parse_string None "class" in
  ()

let suite =
  "TestParser" >::: [
    "test_parser" >:: test_parser;
  ]

let () =
  run_test_tt_main suite
