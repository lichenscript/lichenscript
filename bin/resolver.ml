open Cli_utils
open Core
open Waterlang_lex
open Waterlang_typing
open Waterlang_parsing

exception ParseError of Parse_error.t list
exception TypeCheckError of Type_error.t list
exception FileNotFound of string

let parse_string_to_program file_key content =
  let result = Parser.parse_string file_key content in
  let env = Waterlang_typing.Env.create () in
  let typed_tree =
    match result with
    | Result.Ok program ->
      begin
        (* Ast.pp_program Format.std_formatter program; *)
        let program = Waterlang_typing.Annotate.annotate env program in
        Typecheck.type_check env program;

        let typecheck_errors = Waterlang_typing.Env.errors env in
        if not (List.is_empty typecheck_errors) then (
          raise (TypeCheckError typecheck_errors)
        );

        program
        
      end

    | Result.Error errs ->
      raise (ParseError errs)
  in
  typed_tree

module ModuleMap = Hashtbl.Make(String)

type t = {
  module_map: Module.t ModuleMap.t;
}

let create () = {
  module_map = ModuleMap.create ();
}

let load_library_by_dir env std_dir =
  let abs_path = Filename.realpath std_dir in

  let lib_entry_file = Filename.concat abs_path "lib.wt" in
  (match (Sys.file_exists lib_entry_file) with
  | `No -> raise (FileNotFound lib_entry_file)
  | _ -> ()
  );

  let entry_file_content = In_channel.read_all lib_entry_file in
  let file_key = File_key.LibFile lib_entry_file in
  let typed_tree = parse_string_to_program (Some file_key) entry_file_content in
  
  let _mod = Module.create abs_path typed_tree in
  ModuleMap.set env.module_map ~key:abs_path ~data:_mod

let compile_file_path ~package_name ~std_dir entry_file_path =
  if Option.is_none std_dir then (
    Format.printf "std library is not found\n";
    ignore (exit 1)
  );
  try
    let env = create () in
    load_library_by_dir env (Option.value_exn std_dir);
    let content = In_channel.read_all entry_file_path in
    let file_key = File_key.SourceFile entry_file_path in
    let _typed_tree = parse_string_to_program (Some file_key) content in
    Format.printf "compile %s for %s\n" entry_file_path package_name;
  with
    | FileNotFound path ->
      print_error_prefix ();
      Out_channel.printf "can not resolve file: %s for module %s" path package_name

    | TypeCheckError errors ->
      List.iter
        ~f:(fun err ->
          let err_str = Format.asprintf "%a" Waterlang_typing.Type_error.PP.error err in
          Out_channel.printf "%s\n" err_str
        )
        errors

    | Parse_error.Error errors
    | ParseError errors ->
      List.iter
        ~f:(fun err ->
          let { Parse_error. perr_loc; _ } = err in
          Loc. (
            match perr_loc.source with
            | Some source -> (
              print_error_prefix ();
              let source_str = Format.asprintf "%a" Waterlang_lex.File_key.pp source in
              Out_channel.printf "parse error in %s\n" (TermColor.grey ^ source_str ^ TermColor.reset)
            )
            | None -> ()
          );
          let start = perr_loc.start in
          Format.printf "%d:%d %a\n" start.line start.column Parse_error.PP.error err
        )
        errors

    | Type_error.Error e ->
      Type_error.PP.error Format.str_formatter e;
      let err_str = Format.asprintf "%a" Type_error.PP.error e in
      print_error_prefix ();
      Out_channel.printf "%s\n" err_str

    | e ->
      let string = Exn.to_string e in
      print_error_prefix ();
      Out_channel.printf "%s\n" string
