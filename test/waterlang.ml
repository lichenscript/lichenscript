
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
  | Result.Ok _program ->
    (* Ast.pp_program Format.std_formatter program; *)
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

let parse_string_and_codegen content =
  let result = Parser.parse_string None content in
  let typed_tree =
    match result with
    | Result.Ok program ->
        begin
        (* Ast.pp_program Format.std_formatter program; *)
        let env = Waterlang_typing.Env.create () in
        try (
          let program = Waterlang_typing.Annotate.annotate env program in
          Waterlang_typing.Typecheck.type_check env program;
          program
        ) with Waterlang_typing.Type_error.Error e ->
          let err_str = Waterlang_typing.Type_error.PP.error e in
          assert_failure err_str
        
      end

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
  in
  let p = { Waterlang_typing.Program. tree = typed_tree } in
  let config = Config.debug_default () in
  Codegen.codegen p config

let test_codegen _ =
  let source = "
    function main(a: i32, b: i32) {
      return a + b;
    }
    "
  in
  let result = parse_string_and_codegen source in
  Format.printf "%s" result

let suite =
  "TestParser" >::: [
    "test_parser" >:: test_parser;
    "test_codegen" >:: test_codegen;
  ]

let () =
  run_test_tt_main suite
