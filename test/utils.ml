open Core
open Waterlang_lex
open Waterlang_parsing
open Waterlang_typing

exception ExpectedError of string

let parse_string_to_program content =
  let result = Parser.parse_string None content in
  let ctx = Waterlang_typing.Type_context.create () in
  let module_scope = new Scope.scope ~prev:(Type_context.root_scope ctx) () in
  let env = Waterlang_typing.Env.create ~module_scope ctx in
  let typed_tree =
    match result with
    | Result.Ok { tree = program; _ } ->
        begin
        (* Ast.pp_program Format.std_formatter program; *)
        try (
          let program = Waterlang_typing.Annotate.annotate_program env program in
          let typecheck_errors = Waterlang_typing.Typecheck.type_check ctx program in

          if not (List.is_empty typecheck_errors) then (
            List.iter
              ~f:(fun e ->
                Format.fprintf Format.str_formatter "%a\n" (Waterlang_typing.Type_error.PP.error ~ctx) e
              )
              typecheck_errors
            ;
            let err_str = Format.flush_str_formatter () in
            raise (ExpectedError err_str)
          );

          program
        ) with Waterlang_typing.Type_error.Error e ->
          let err_str = Format.asprintf "%a" (Waterlang_typing.Type_error.PP.error ~ctx) e in
          raise (ExpectedError err_str)
        
      end

    | Result.Error errs ->
      errs
      |> List.rev
      |> List.iter
        ~f:(fun error ->
          let str = Format.asprintf "%a" Parse_error.PP.error error in
          let { Loc. line; column; } = error.perr_loc.start in
          Format.printf "%d:%d %s\n" line column str;
          );
      assert false
  in
  ctx, typed_tree

let parse_string_and_codegen content =
  let ctx, p = parse_string_to_program content in
  Waterlang_c.codegen ~ctx p

(* let parse_string_and_codegen_to_path content path =
  let p = parse_string_to_program content in
  let config = Config.debug_default () in
  let slices = String.split path ~on:'/' in
  let output_filename = List.last_exn slices in
  let env = Codegen_env.create ~output_filename config p in
  Codegen.codegen_binary env path *)
