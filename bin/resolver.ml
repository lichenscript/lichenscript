open Cli_utils
open Core
open Waterlang_lex
open Waterlang_typing
open Waterlang_parsing

exception ParseError of Parse_error.t list
exception TypeCheckError of Type_error.t list

let parse_string_to_typed_tree ~file_key content =
  let result = Parser.parse_string file_key content in
  let env = Waterlang_typing.Env.create () in
  let typed_tree =
    match result with
    | Result.Ok { tree; _ } ->
      begin
        (* Ast.pp_program Format.std_formatter program; *)
        Waterlang_typing.Annotate.annotate env tree
      end

    | Result.Error errs ->
      raise (ParseError errs)
  in
  typed_tree

(* let parse_string_to_program ~file_key ~type_provider content =
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
  typed_tree, include_module_ids *)

module ModuleMap = Hashtbl.Make(String)

type t = {
  module_map: Module.t ModuleMap.t;
  find_paths: string list;
}

let create ~find_paths () = {
  module_map = ModuleMap.create ();
  find_paths;
}

let create_type_provider env : Type_provider.provider =
  object

    method resolve (mod_arr, local_arr) =
      let mod_id = Module.get_id_str mod_arr in
      match ModuleMap.find env.module_map mod_id with
      | Some _mod -> (
        let files = Module.files _mod in
        List.fold_until
          ~init:None
          ~f:(fun _ file ->
            let typed_tree = Module.(file.typed_tree) in
            let { Typedtree. root_scope; _ } = typed_tree in
            if Array.length local_arr = 1 then (
              let first_name = Array.get local_arr 0 in
              match Scope.find_var_symbol root_scope first_name with
              | Some sym -> Base.Continue_or_stop.Stop (Some sym)
              | None -> Base.Continue_or_stop.Continue None
            ) else
              Base.Continue_or_stop.Continue None
          )
          ~finish:(fun item -> item)
          files
      )
      | None -> None

  end

let last_piece_of_path path =
  let parts = Filename.parts path in
  List.last_exn parts

(* let rec load_library_by_dir id_list env std_dir =
  let abs_path = Filename.realpath std_dir in

  let lib_name = last_piece_of_path abs_path in

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
  ) *)

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

let allow_suffix = Re.Pcre.regexp "^(.+)\\.wt$"

let create_or_insert_moudule_file env ~id ~id_str file =
  match ModuleMap.find env.module_map id_str with
  | Some m ->
    Module.add_file m file

  | None -> (
    let m = Module.create ~id ~id_str () in
    Module.add_file m file;
    ModuleMap.set env.module_map ~key:id_str ~data:m
  )

let compile_file_to_path env names path =
  let file_content = In_channel.read_all path in
  let file_key = File_key.LibFile path in
  let typed_tree = parse_string_to_typed_tree ~file_key:(Some file_key) file_content in
  (* let filename = Filename.basename path in
  let test_regex = Re.exec allow_suffix filename in
  let mod_name =  *)
  let id = names |> List.rev |> List.to_array in
  let module_id_str = Module.get_id_str id in
  let file =
    { Module.
      path;
      typed_tree;
    }
  in
  create_or_insert_moudule_file env ~id ~id_str:module_id_str file

(* recursive all files in the path *)
let parse_and_annotate_path env path =
  let rec iterate names path =
    let children = Sys.ls_dir path in
    List.iter
      ~f:(fun item ->
        let child_path = Filename.concat path item in
        if Sys.is_file_exn child_path then (
          let test_result = Re.exec allow_suffix child_path |> Re.Group.all in
          if Array.length test_result > 1 then  (* is a .wt file *)
            compile_file_to_path env names child_path
          else ()
        ) else if Sys.is_directory_exn child_path then (
          let mod_name = last_piece_of_path child_path in
          iterate (mod_name::names) child_path
        )
      )
      children
  in
  let mod_root_name = last_piece_of_path path in
  iterate [mod_root_name] path

let parse_and_annotate_find_paths env =
  List.iter ~f:(parse_and_annotate_path env) env.find_paths

let typecheck_all_modules env =
  let type_provider = create_type_provider env in
  ModuleMap.iter
    ~f:(fun m -> 
      let files = Module.files m in
      let files =
        List.map
          ~f:(fun file ->
            let typed_tree, errors = Typecheck.type_check ~type_provider file.typed_tree in
            if (List.length errors) > 0 then (
              raise (TypeCheckError errors)
            );
            { file with typed_tree }
          )
          files
      in
      Module.set_files m files
    )
    env.module_map

(*
 * 1. parse all files with .wt of find path
 * 2. parse all program files with .wt of find path
 * 3. type check one by one
 *
 * All the files should be annotated before type check,
 * because cyclic dependencies is allowed.
 * Annotated parsed tree remain the "holes" to type check
 *)
let rec compile_file_path ~std_dir ~build_dir entry_file_path =
  if Option.is_none std_dir then (
    Format.printf "std library is not found\n";
    ignore (exit 1)
  );
  try
    let env = create ~find_paths:[ Option.value_exn std_dir ] () in

    (* load standard library *)
    (* parse_and_annotate_find_paths *)
    parse_and_annotate_find_paths env;

    (* parse the entry file *)
    let dir_of_entry = Filename.dirname entry_file_path in
    parse_and_annotate_path env dir_of_entry;

    typecheck_all_modules env;

    (* open std.preclude to module scope *)
    (* let content = In_channel.read_all entry_file_path in
    let file_key = File_key.SourceFile entry_file_path in *)

    let main_mod = ModuleMap.find_exn env.module_map "hello_world" in
    let file = List.hd_exn (Module.files main_mod) in
    
    let typed_tree = Module.(file.typed_tree) in

    (* TODO: compile other modules *)
    let output = Waterlang_c.codegen typed_tree in
    let mod_name = entry_file_path |> Filename.dirname |> last_piece_of_path in
    let output_path = write_to_file build_dir mod_name output in
    let build_dir = Option.value_exn build_dir in
    write_runtime_files build_dir;
    let bin_name = entry_file_path |> last_piece_of_path |> (Filename.chop_extension) in
    write_makefiles ~bin_name build_dir [ (mod_name, output_path) ];
    run_make_in_dir build_dir;
  with
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

and write_to_file build_dir mod_name content: string =
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
  Out_channel.write_all output_file_path ~data:content;
  output_file_path

and write_runtime_files build_dir =
  List.iter
    ~f:(fun (name, content) ->
      let output_path = Filename.concat build_dir name in
      Out_channel.write_all output_path ~data:content
    )
    Embed.contents

and write_makefiles ~bin_name build_dir mods =
  let output_path = Filename.concat build_dir "Makefile" in
  let open Makefile in
  let c_srcs = List.fold ~init:"runtime.c" ~f:(fun acc (m, _) -> (acc ^ " " ^ m ^ ".c")) mods in
  let entries = List.concat [
    [
      {
        entry_name = "all";
        deps = List.concat [ ["runtime"]; (List.map ~f:(fun (m, _) -> m) mods)];
        content = Format.sprintf "cc %s -o %s" c_srcs bin_name;
      };
      {
        entry_name = "runtime";
        deps = ["runtime.c"; "runtime.h"];
        content = "cc -c runtime.c";
      }
    ];
    List.map
      ~f:(fun (m, output) -> {
        entry_name = m;
        deps = [];
        content = "cc -c " ^ (Filename.basename output)
      })
      mods;
  ] in
  let data = to_string entries in
  Out_channel.write_all output_path ~data

and run_make_in_dir build_dir =
  Out_channel.printf "Spawn to build in %s\n" (TermColor.bold ^ build_dir ^ TermColor.reset);
  Out_channel.flush Out_channel.stdout;
  Out_channel.flush Out_channel.stderr;
  match Unix.fork () with
  | `In_the_child -> 
    Unix.chdir build_dir;
    Unix.exec ~prog:"make" ~argv:["make";] () |> ignore

  | `In_the_parent pid ->
    ignore (Unix.waitpid pid)
