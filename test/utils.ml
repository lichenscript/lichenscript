open OUnit2
open Waterlang_lex
open Waterlang_parsing
open Waterlang_wasm

let parse_string_to_program content =
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
  { Waterlang_typing.Program. tree = typed_tree }

let parse_string_and_codegen content =
  let p = parse_string_to_program content in
  let config = Config.debug_default () in
  Codegen.codegen p config

let parse_string_and_codegen_to_path content path =
  let p = parse_string_to_program content in
  let config = Config.debug_default () in
  Codegen.codegen_binary p config path
