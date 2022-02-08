(*
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)
open Core
open Lichenscript_lex
open Lichenscript_typing
open Lichenscript_parsing

exception ParseError of Parse_error.t list
exception TypeCheckError of Type_error.t list

type t = {
  (* absolute path => module *)
  linker: Linker.t;
  find_paths: string list;
}

let create ~find_paths ~ctx () =
  let linker = Linker.create ~ctx () in
  {
    linker;
    find_paths;
  }

class[@warning "-unused-ancestor"] module_scope ~prev () = object
  inherit Scope.scope ~prev () as super

end

class file_scope ~prev env extern_modules = object
  inherit Scope.scope ~prev () as super

  method! set_variable_captured _level (_name: string) = false

  method! insert_var_symbol name var =
    prev#insert_var_symbol name var

  method! new_var_symbol ~id ~kind name =
    prev#new_var_symbol ~id ~kind name 

  method! init_symbol name =
    prev#init_symbol name

  method! insert_type_symbol name sym =
    prev#insert_type_symbol name sym

  method! next_var_id = prev#next_var_id

  method! vars = prev#vars

  method! set_visibility name v =
    prev#set_visibility name v

  method! get_visibility name = prev#get_visibility name

  (* override *)
  method! find_var_symbol (name: string): Scope.variable option =
    let module_result = super#find_var_symbol name in
    match module_result with
    | Some _ -> module_result
    | None -> (
      (* not in the local and prev, find the symbol in other modules *)
      List.fold_until
        ~init:None
        ~f:(fun acc extern_mod ->
          if Option.is_some acc then
            Base.Continue_or_stop.Stop acc
          else
            try
              let _mod = Option.value_exn (Linker.get_module env.linker extern_mod) in
              let export = Module.find_export _mod ~name in
              let open Module in
              match export with
              | Some export ->
                Base.Continue_or_stop.Stop (Some export.export_var)
              | None ->
                Base.Continue_or_stop.Continue None
            with
            | _ ->
              Format.eprintf "unexpected: find module failed: %s\n" extern_mod;
              Base.Continue_or_stop.Continue None
        )
        ~finish:(fun item -> item)
        extern_modules
    )

  method! find_type_symbol (name: string): int option =
    let module_result = super#find_type_symbol name in
    match module_result with
    | Some _ -> module_result
    | None -> (
      (* not in the local and prev, find the symbol in other modules *)
      List.fold_until
        ~init:None
        ~f:(fun acc extern_mod ->
          if Option.is_some acc then
            Base.Continue_or_stop.Stop acc
          else
            try
              let _mod = Option.value_exn (Linker.get_module env.linker extern_mod) in
              let export = Module.find_export _mod ~name in
              let open Module in
              match export with
              | Some export ->
                Base.Continue_or_stop.Stop (Some export.export_var.var_id)
              | None ->
                Base.Continue_or_stop.Continue None
            with
            | _ ->
              Format.eprintf "unexpected: find module failed: %s\n" extern_mod;
              Base.Continue_or_stop.Continue None
        )
        ~finish:(fun item -> item)
        extern_modules
    )

end

let last_piece_of_path path =
  let parts = Filename.parts path in
  List.last_exn parts

let allow_suffix = Re.Pcre.regexp "^(.+)\\.lc$"

let insert_moudule_file env ~mod_path file =
  match Linker.get_module env.linker mod_path with
  | Some m ->
    Module.add_file m file

  | None -> (
    failwith (Format.sprintf "unexpected: can not find mod %s" mod_path)
  )

let rec compile_file_to_path ~ctx ~mod_path env _mod path =
  let file_content = In_channel.read_all path in
  let file_key = File_key.LibFile path in
  let ast =
    match Parser.parse_string (Some file_key) file_content with
    | Result.Ok ast -> ast
    | Result.Error err ->
      raise (ParseError err)
  in

  let imports = Ast.(
    let collected_imports =
    List.fold
      ~init:[]
      ~f:(fun acc item ->
        let open Ast.Declaration in
        match item.spec with
        | Import import -> import::acc
        | _ -> acc
      )
      ast.tree.pprogram_declarations
    in
    let preclude = {
      Ast.Declaration.
      source = "std/preclude";
      source_loc = Loc.none;
    } in
    List.rev (preclude::collected_imports)
  ) in

  let extern_modules = ref [] in

  List.iter
    ~f:(fun import ->
      let open Ast.Declaration in
      let { source; _ } = import in
      let find_paths = env.find_paths in
      let result =
        List.fold
          ~init:None
          ~f:(fun acc path ->
            match acc with
            | Some _ -> acc
            | None ->
              let path = Filename.concat path source in
              if Sys.is_directory_exn path then (
                Some path
              ) else None
          )
          find_paths in
      match result with
      | Some path -> (
        match parse_module_by_dir ~ctx env path with
        | Some full_path ->
          extern_modules := full_path::!extern_modules;
        | None -> ()
      )
      | None ->
        failwith (Format.sprintf "can not find module %s" source)
    )
    imports;

  let extern_modules = List.rev !extern_modules in

  let module_scope = Module.module_scope _mod in
  let file_scope = new file_scope ~prev:module_scope env extern_modules in
  (* parse and create env, do annotation when all files are parsed
   * because annotation stage needs all exported symbols are resolved
   *)
  let typed_env = Lichenscript_typing.Env.create ~file_scope ctx in

  (* add all top level symbols to typed_env *)
  Tree_helper.add_top_level_symbols_to_typed_env typed_env ast.tree;

  let file =
    { Module.
      path;
      ast = Some ast.tree;
      typed_env;
      typed_tree = None;
      extern_modules;
    }
  in
  insert_moudule_file env ~mod_path file

(* recursive all files in the path *)
and parse_module_by_dir ~ctx env dir_path : string option =
  let iterate_parse_file mod_path =
    let module_scope = new module_scope ~prev:(Type_context.root_scope ctx) () in
    let _mod = Module.create ~full_path:mod_path ~module_scope () in
    Linker.set_module env.linker mod_path _mod;
    let children = Sys.ls_dir mod_path in
    (* only compile files in this level *)
    List.iter
      ~f:(fun item ->
        let child_path = Filename.concat mod_path item in
        if Sys.is_file_exn child_path then (
          try[@alert "-deprecated"]  (* disable the deprecated alert *)
            let test_result = Re.exec allow_suffix child_path |> Re.Group.all in
            if Array.length test_result > 1 then ((* is a .lc file *)
              compile_file_to_path ~ctx ~mod_path env _mod child_path
            )
          with
          | Not_found -> ()
        ) else ()
      )
      children;
    Module.finalize_module_exports _mod;
  in
  let full_path = Filename.realpath dir_path in
  if not (Linker.has_module env.linker full_path) then (
    iterate_parse_file full_path;
    Some full_path
  ) else
    None

let annotate_all_modules env =
  Linker.iter_modules
    ~f:(fun m ->
      let files = Module.files m in
      let files =
        List.map
          ~f:(fun file -> 
            let { Module. typed_env; ast; _ } = file in
            let typed_tree =
              Lichenscript_typing.Annotate.annotate_program
              typed_env (Option.value_exn ast)
            in
            { file with
              (* clear the ast to released memory,
               * but don't know if there are other references
               *)
              ast = None;
              typed_tree = Some typed_tree;
            }
          )
          files
      in
      Module.set_files m files
    )
    env.linker

let typecheck_all_modules ~ctx ~debug env =
  annotate_all_modules env;
  Linker.iter_modules
    ~f:(fun m ->
      let files = Module.files m in
      List.iter
      ~f:(fun file ->
        let open Module in
        let tree = Option.value_exn file.typed_tree in
        Typecheck.typecheck_module ~debug ctx tree
      )
      files;
      (* Typecheck.typecheck_module ctx m. *)
    )
    env.linker

(*
 * 1. parse all in the entry dir
 * 2. annotate all files
 * 3. type check one by one
 *
 * All the files should be annotated before type check,
 * because cyclic dependencies is allowed.
 * Annotated parsed tree remain the "holes" to type check
 *)
let rec compile_file_path ~std_dir ~build_dir ~runtime_dir ~debug entry_file_path =
  if Option.is_none std_dir then (
    Format.printf "std library is not found\n";
    ignore (exit 1)
  );
  if Option.is_none runtime_dir then (
    Format.printf "runtime library is not found\n";
    ignore (exit 1)
  );
  try
    (* ctx is a typing context for all modules *)
    let ctx = Lichenscript_typing.Type_context.create () in
    let env = create ~find_paths:[ Option.value_exn std_dir ] ~ctx () in

    (* parse the entry dir *)
    let dir_of_entry = Filename.dirname entry_file_path in
    let entry_full_path = parse_module_by_dir ~ctx env dir_of_entry in

    typecheck_all_modules ~ctx ~debug env;

    (* open std.preclude to module scope *)

    let main_mod = Option.value_exn (Linker.get_module env.linker (Option.value_exn entry_full_path)) in
    if List.is_empty (Module.files main_mod) then (
      Format.printf "No files should be compiled\n";
      ignore (exit 0)
    );
    let file = List.hd_exn (Module.files main_mod) in
    
    let typed_tree: Typedtree.program = Option.value_exn Module.(file.typed_tree) in

    let main_fun_id = typed_tree.tprogram_scope#find_type_symbol "main" in
    if Option.is_none main_fun_id then (
      Format.printf "main function is not found, nothing to output";
      ignore (exit 0)
    );

    let declarations = Linker.link_from_entry env.linker ~debug (Option.value_exn main_fun_id) in

    let output = Lichenscript_c.codegen ~ctx declarations in
    let mod_name = entry_file_path |> Filename.dirname |> last_piece_of_path in
    let output_path = write_to_file build_dir mod_name output in
    let build_dir = Option.value_exn build_dir in
    let bin_name = entry_file_path |> last_piece_of_path |> (Filename.chop_extension) in
    write_makefiles ~bin_name ~runtime_dir:(Option.value_exn runtime_dir) build_dir [ (mod_name, output_path) ];
    build_dir, (Some (Filename.concat build_dir bin_name))
  with
    | Type_error.Error e ->
      raise (TypeCheckError [e])

    | Parse_error.Error errors ->
      raise (ParseError errors)

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

and write_makefiles ~bin_name ~runtime_dir build_dir mods =
  let output_path = Filename.concat build_dir "Makefile" in
  let open Makefile in
  let runtime_dir = Filename.concat runtime_dir "c" in
  let c_srcs = List.fold ~init:"runtime.o" ~f:(fun acc (m, _) -> (acc ^ " " ^ m ^ ".o")) mods in
  let entries = List.concat [
    [
      {
        entry_name = "all";
        deps = List.concat [ ["runtime"]; (List.map ~f:(fun (m, _) -> m) mods)];
        content = Format.sprintf "cc %s -o %s" c_srcs bin_name;
      };
      {
        entry_name = "runtime";
        deps =
          List.map
          ~f:(fun name ->
            (Filename.concat runtime_dir name)
            |> Filename.realpath
          )
          ["runtime.c"; "runtime.h"];
        content = "cc -c " ^ Filename.(concat runtime_dir "runtime.c"|> realpath);
      }
    ];
    List.map
      ~f:(fun (m, output) -> {
        entry_name = m;
        deps = [];
        content = Format.sprintf "cc -I%s -c %s" (Filename.realpath runtime_dir) (Filename.basename output);
      })
      mods;
  ] in
  let data = to_string entries in
  Out_channel.write_all output_path ~data
