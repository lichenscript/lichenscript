open Cli_utils
open Core
open Waterlang_lex
open Waterlang_typing
open Waterlang_parsing

exception ParseError of Parse_error.t list
exception TypeCheckError of Type_error.t list
exception FileNotFound of string

let parse_string_to_program ~file_key ~type_provider content =
  let result = Parser.parse_string file_key content in
  let open_domains = [
    [| "std"; "preclude" |]
  ] in
  let env = Waterlang_typing.Env.create () ~open_domains ~type_provider in
  let typed_tree, include_module_ids =
    match result with
    | Result.Ok { tree; include_module_ids } ->
      begin
        (* Ast.pp_program Format.std_formatter program; *)
        let program = Waterlang_typing.Annotate.annotate env tree in
        Typecheck.type_check env program;

        let typecheck_errors = Waterlang_typing.Env.errors env in
        if not (List.is_empty typecheck_errors) then (
          raise (TypeCheckError typecheck_errors)
        );

        (program, include_module_ids)
        
      end

    | Result.Error errs ->
      raise (ParseError errs)
  in
  typed_tree, include_module_ids

module ModuleMap = Hashtbl.Make(String)

type t = {
  module_map: Module.t ModuleMap.t;
}

let create () = {
  module_map = ModuleMap.create ();
}

let get_mod_id id_list =
  Module.get_id_str (List.rev id_list |> List.to_array)

let create_type_provider env : Type_provider.provider =
  object

    method resolve (mod_arr, local_arr) =
      let mod_id = Module.get_id_str mod_arr in
      match ModuleMap.find env.module_map mod_id with
      | Some _mod -> (
        let typed_tree = Module.typed_tree _mod in
        let { Typedtree. root_scope; _ } = typed_tree in
        if Array.length local_arr = 1 then (
          let first_name = Array.get local_arr 0 in
          Scope.find_var_symbol root_scope first_name
        ) else
          None
      )
      | None -> None

  end

let dirname path =
  let parts = Filename.parts path in
  List.last_exn parts

let rec load_library_by_dir id_list env std_dir =
  let abs_path = Filename.realpath std_dir in

  let lib_name = dirname abs_path in

  let module_id = lib_name::id_list in
  let module_id_str = get_mod_id module_id in
  match ModuleMap.find env.module_map module_id_str with
  | Some _ -> ()
  | None -> (
    let lib_entry_file = Filename.concat abs_path "lib.wt" in
    (match (Sys.file_exists lib_entry_file) with
    | `No -> raise (FileNotFound lib_entry_file)
    | _ -> ()
    );

    let entry_file_content = In_channel.read_all lib_entry_file in
    let file_key = File_key.LibFile lib_entry_file in
    let type_provider = create_type_provider env in
    let typed_tree, child_modules =
      parse_string_to_program ~file_key:(Some file_key) ~type_provider entry_file_content
    in
    let id = List.rev (lib_name::id_list) |> List.to_array in
    let _mod = Module.create ~path:abs_path ~id ~id_str:module_id_str typed_tree in
    ModuleMap.set env.module_map ~key:module_id_str ~data:_mod;
    List.iter
      ~f:(fun item ->
        load_library_by_dir module_id env (Filename.concat std_dir item)
      )
      child_modules
  )

let print_loc_title ~prefix loc_opt =
  Loc. (
    match loc_opt.source with
    | Some source -> (
      print_error_prefix ();
      let source_str = Format.asprintf "%a" Waterlang_lex.File_key.pp source in
      Out_channel.printf "%s in %s\n" prefix (TermColor.bold ^ source_str ^ TermColor.reset)
    )
    | None -> ()
  )

let rec compile_file_path ~package_name ~std_dir ~build_dir entry_file_path =
  if Option.is_none std_dir then (
    Format.printf "std library is not found\n";
    ignore (exit 1)
  );
  try
    let env = create () in
    (* load standard library *)
    load_library_by_dir [] env (Option.value_exn std_dir);
    (* open std.preclude to module scope *)
    let content = In_channel.read_all entry_file_path in
    let file_key = File_key.SourceFile entry_file_path in
    let type_provider = create_type_provider env in
    let typed_tree, _ =
      parse_string_to_program ~file_key:(Some file_key) ~type_provider content
    in
    (* TODO: compile other modules *)
    let output = Waterlang_c.codegen typed_tree in
    let mod_name = entry_file_path |> Filename.dirname |> dirname in
    write_to_file build_dir mod_name output
  with
    | FileNotFound path ->
      print_error_prefix ();
      Out_channel.printf "can not resolve file: %s for module %s" path package_name

    | TypeCheckError errors ->
      List.iter
        ~f:(fun err ->
          let { Type_error. spec; loc } = err in
          print_loc_title ~prefix:"type error" loc;
          let start = loc.start in
          Format.printf "%d:%d %a\n" start.line start.column Type_error.PP.error_spec spec
        )
        errors

    | Parse_error.Error errors
    | ParseError errors ->
      List.iter
        ~f:(fun err ->
          let { Parse_error. perr_loc; _ } = err in
          print_loc_title ~prefix:"parse error" perr_loc;
          let start = perr_loc.start in
          Format.printf "%d:%d %a\n" start.line start.column Parse_error.PP.error err
        )
        errors

    | Type_error.Error e ->
      let { Type_error. spec; loc } = e in
      print_loc_title ~prefix:"type error" loc;
      let start = loc.start in
      Format.printf "%d:%d %a\n" start.line start.column Type_error.PP.error_spec spec

    (* | e ->
      let string = Exn.to_string e in
      print_error_prefix ();
      Out_channel.printf "%s\n" string;
      let stack = Printexc.get_backtrace () in
      Out_channel.print_string TermColor.grey;
      Out_channel.print_string stack;
      Out_channel.print_string TermColor.reset;
      Out_channel.print_endline "" *)

and write_to_file build_dir mod_name content =
  let build_dir =
    match build_dir with
    | Some v -> v
    | None -> Filename.concat Filename.temp_dir_name "waterlang"
  in
  (match Sys.file_exists build_dir with
  | `No -> (
    Unix.mkdir_p build_dir
  )
  | _ -> ()
  );
  let output_file_path = Filename.concat build_dir (mod_name ^ ".c") in
  Out_channel.write_all output_file_path ~data:content
