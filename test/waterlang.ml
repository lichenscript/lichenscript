
open OUnit2
open Waterlang_lex
open Waterlang_parsing

let test_parser _ =
  let result =  Parser.parse_string None "class Array { }" in
  match result with
  | Result.Ok program ->
    Ast.pp_program Format.std_formatter program;
    assert true

  | Result.Error errs ->
    List.iter
      (fun error ->
        let str = Parse_error.PP.error error in
        let { Loc. line; column; } = error.perr_loc.start in
        Format.printf "%d:%d %s\n" line column str;
      )
      errs;
    assert false

let suite =
  "TestParser" >::: [
    "test_parser" >:: test_parser;
  ]

let () =
  run_test_tt_main suite
