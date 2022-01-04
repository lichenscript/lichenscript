open Core
open Waterlang_lex
open Waterlang_parsing

exception ExpectedError of string

let parse_string_to_program content =
  let result = Parser.parse_string None content in
  let env = Waterlang_typing.Env.create () in
  let typed_tree =
    match result with
    | Result.Ok program ->
        begin
        (* Ast.pp_program Format.std_formatter program; *)
        try (
          let program = Waterlang_typing.Annotate.annotate env program in
          Waterlang_typing.Typecheck.type_check env program;

          let typecheck_errors = Waterlang_typing.Env.errors env in
          if not (List.is_empty typecheck_errors) then (
            List.iter
              ~f:(fun e ->
                Format.fprintf Format.str_formatter "%a\n" Waterlang_typing.Type_error.PP.error e
              )
              typecheck_errors
            ;
            let err_str = Format.flush_str_formatter () in
            raise (ExpectedError err_str)
          );

          program
        ) with Waterlang_typing.Type_error.Error e ->
          Waterlang_typing.Type_error.PP.error Format.str_formatter e;
          let err_str = Format.flush_str_formatter () in
          raise (ExpectedError err_str)
        
      end

    | Result.Error errs ->
      errs
      |> List.rev
      |> List.iter
        ~f:(fun error ->
          let str = Parse_error.PP.error error in
          let { Loc. line; column; } = error.perr_loc.start in
          Format.printf "%d:%d %s\n" line column str;
          );
      assert false
  in
  typed_tree

let rec build_task ~package_name ~entry_dir task =
  let { Task. entry_file; _ } = task in
  let entry_file_path = Filename.concat entry_dir entry_file in
  compile_file_path ~package_name entry_file_path

and compile_file_path ~package_name entry_file_path =
  let content = In_channel.read_all entry_file_path in
  let _typed_tree = parse_string_to_program content in
  Format.printf "compie %s for %s\n" entry_file_path package_name;
  ()
