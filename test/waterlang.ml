
open OUnit2
open Waterlang_lex
open Waterlang_parsing
open Waterlang_wasm

let test_parser _ =
  let result =  Parser.parse_string None "
  class Array {
    private __name: sring;
  }

  function main(args, args2: string, ...rest) {
    let name = 3;
  }
  " in
  match result with
  | Result.Ok program ->
    Ast.pp_program Format.std_formatter program;
    let _env = Waterlang_typing.Env.create () in
    (* let program = Waterlang_typing.Annotate.annotate env program in
    Waterlang_typing.Typecheck.type_check env program; *)
    assert true

  | Result.Error errs ->
    errs
    |> List.rev
    |> List.iter
       (fun error ->
         let str = Parse_error.PP.error error in
         let { Loc. line; column; } = error.perr_loc.start in
         Format.printf "%d:%d %s\n" line column str;
        );
    assert false

let test_codegen _ =
  Codegen.codegen ();
  assert true

let suite =
  "TestParser" >::: [
    "test_parser" >:: test_parser;
    "test_codegen" >:: test_codegen;
  ]

let () =
  run_test_tt_main suite
