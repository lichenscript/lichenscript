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
open Core_kernel
open Lichenscript_lex
open Lichenscript_typing
open Lichenscript_parsing

exception ParseError of Parse_error.t list
exception TypeCheckError of Diagnosis.t list
exception ResolveError of Resolve_error.t

module type FSProvider = sig

  val is_directory: string -> bool

  val is_file: string -> bool

  val get_realpath: string -> string

  val ls_dir: string -> string list

  val mkdir_p: string -> unit

  val file_exists: string -> bool

  val read_file_content: string -> string

  val write_file_content: string -> data:string -> unit

end

module S (FS: FSProvider) = struct

  type config = {
    find_paths: string list;
    build_dir: string option;
    runtime_dir: string;
    platform: string;
    verbose: bool;
    wasm_standalone: bool;
  }

  type t = {
    (* absolute path => module *)
    linker: Linker.t;
    config: config;
    mutable before_eval_fun_call: string list;
  }

  type profile = {
    profile_name: string;
    profile_dir: string;
    profile_exe_path: string;
  }

  let create ~prog ~config () =
    let linker = Linker.create ~prog () in
    {
      linker;
      config;
      before_eval_fun_call = [];
    }

  class module_scope ~prev () = object
    inherit Scope.scope ~prev ()

  end

  (*
   * extern_modules are absolute path of external modules
   * the absolute path of module's folder is the unique id of the module
   *)
  class file_scope ~prev env extern_modules = object
    inherit Scope.scope ~prev () as super

    method! set_variable_captured _level (_name: string) = false

    method! insert_var_symbol name var =
      prev#insert_var_symbol name var

    method! new_var_symbol ~id ~kind ~loc name =
      prev#new_var_symbol ~id ~kind ~loc name 

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

  let preclude_std_for_imports (ast: Parser.parse_result) =
    let open Ast in
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
    let preclude = { Ast.Import.
      spec = Some ImportAll;
      source = "std/preclude";
      source_loc = Loc.none;
    } in
    List.rev (preclude::collected_imports)

  let resolve_import_path env ~mod_path source =
    let find_paths = mod_path::(env.config.find_paths) in
    List.find_map
      ~f:(fun path ->
        if Filename.is_absolute source then
          Some (source, source)
        else (
          let path = Filename.concat path source in
          if FS.is_directory path then (
            Some (FS.get_realpath path, source)
          ) else None
        )
      )
      find_paths

  let rec compile_file_to_path ~prog ~mod_path env _mod path =
    let file_content = FS.read_file_content path in
    let file_key = File_key.LibFile path in
    let ast =
      match Parser.parse_string (Some file_key) ~filter_platform:env.config.platform file_content with
      | Result.Ok ast -> ast
      | Result.Error errors ->
        raise (ParseError errors)
    in

    let imports = preclude_std_for_imports ast in

    let import_star_external_modules = ref [] in
    let imports_map = Hashtbl.create (module String) in

    let find_path = resolve_import_path env ~mod_path in

    let handle_import_lc_module import spec =
      let open Ast.Import in
      let { source; source_loc; _ } = import in
      let result = find_path source in
      match result with
      | Some (path, source) -> (
        ignore (parse_module_by_dir ~prog env ~real_path:path source);
        Hashtbl.set imports_map ~key:source ~data:path;
        match spec with
        | ImportAll ->
          import_star_external_modules := path::!import_star_external_modules;
        | _ -> ()
      )
      | None -> (
        let err = { Resolve_error.
          spec = CannotResolve source;
          loc = source_loc
        } in
        raise (ResolveError err)
      )
    in

    let handle_import_resource source source_loc =
      let _, ext_opt = Filename.split_extension source in
      match ext_opt with
      | Some "js"
      | Some "h"
      | Some "c" -> (
        let normalized_path =
          if Char.((String.get source 0) = '/') then
            source
          else
            Filename.concat mod_path source
        in
        Linker.add_external_resource env.linker normalized_path
      )

      | Some _ -> (
        let err = { Resolve_error.
          spec = CannotResolve source;
          loc = source_loc
        } in
        raise (ResolveError err)
      )
      | None -> (
        let err = { Resolve_error.
          spec = CannotfindExtOfUnivertialImport source;
          loc = source_loc
        } in
        raise (ResolveError err)
      )
    in

    List.iter
      ~f:(fun import ->
        let open Ast.Import in
        let { source; source_loc; spec; _ } = import in
        match spec with
        | Some spec -> handle_import_lc_module import spec
        | None -> handle_import_resource source source_loc
      )
      imports;

    let import_star_external_modules = List.rev !import_star_external_modules in

    let module_scope = Module.module_scope _mod in
    let file_scope = new file_scope ~prev:module_scope env import_star_external_modules in
    (* parse and create env, do annotation when all files are parsed
    * because annotation stage needs all exported symbols are resolved
    *)
    let typed_env = Lichenscript_typing.Env.create
      ~file_scope
      ~external_resolver:(external_resolver env imports_map)
      prog
    in

    (* add all top level symbols to typed_env *)
    Tree_helper.add_top_level_symbols_to_typed_env typed_env ast.tree;

    let file =
      { Module.
        path;
        ast = Some ast.tree;
        typed_env;
        typed_tree = None;
        import_star_external_modules;
        imports_map;
      }
    in
    insert_moudule_file env ~mod_path file

  (*
   * recursive all files in the path
   *
   * @param real_path must be an absolute path
   *)
  and parse_module_by_dir ~prog env ~real_path:dir_path first_source : string option =
    let iterate_parse_file mod_path =
      let module_scope = new module_scope ~prev:(Program.root_scope prog) () in
      let is_std = String.equal first_source "std/preclude" in
      let _mod = Module.create ~full_path:mod_path ~is_std ~module_scope () in
      Linker.set_module env.linker mod_path _mod;
      let children = FS.ls_dir mod_path in
      (* only compile files in this level *)
      List.iter
        ~f:(fun item ->
          let child_path = Filename.concat mod_path item in
          if FS.is_file child_path then (
            try[@alert "-deprecated"]  (* disable the deprecated alert *)
              let test_result = Re.exec allow_suffix child_path |> Re.Group.all in
              if Array.length test_result > 1 then ((* is a .lc file *)
                compile_file_to_path ~prog ~mod_path env _mod child_path
              )
            with
            | Not_found -> ()
          ) else ()
        )
        children;
      Module.finalize_module_exports _mod;
    in
    if not (Linker.has_module env.linker dir_path) then (
      iterate_parse_file dir_path;
      Some dir_path
    ) else
      None

  and external_resolver env imports_map mod_id ~name =
    let open Module in
    let open Option in
    let full_path_opt = Hashtbl.find imports_map mod_id in
    full_path_opt >>= fun full_path ->
    let mod_opt = Linker.get_module env.linker full_path in
    mod_opt >>= fun _mod ->
    let export_opt = Module.find_export _mod ~name in
    export_opt >>| fun export ->
    export.export_var.var_id

  let annotate_all_modules env =
    Linker.iter_modules
      ~f:(fun m ->
        let files = Module.files m in
        let files =
          List.map
            ~f:(fun file -> 
              let { Module. typed_env; ast; _ } = file in
              let typed_tree = Lichenscript_typing.Annotate.annotate_program
                typed_env
                (Option.value_exn ast)
              in

              env.before_eval_fun_call <- List.append env.before_eval_fun_call typed_tree.tprogram_before_eval_fun_call;

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

  let import_checker env _mod file (import: Ast.Import.t) =
    match import.spec with
    | Some Ast.Import.ImportAll -> (
      let { Module. imports_map; _ } = file in
      let module_full_path = Hashtbl.find_exn imports_map import.source in
      let _module = Option.value_exn (Linker.get_module env.linker module_full_path) in
      let exports = Module.exports _module in
      let module_scope = Module.module_scope _mod in

      List.iter
        ~f:(fun (export_name, _) -> 
          let test_variable = module_scope#find_var_symbol export_name in
          match test_variable with
          | Some _variable ->
            let err = { Resolve_error.
              spec = Redeclared export_name;
              (* loc = variable.var_loc; *)
              loc = import.source_loc;
            } in
            raise (ResolveError err)
          | None -> ()
        )
        exports
      )
    | _ -> ()

  let typecheck_all_modules ~prog ~verbose env =
    annotate_all_modules env;
    Linker.iter_modules
      ~f:(fun m ->
        let files = Module.files m in
        List.iter
        ~f:(fun file ->
          let open Module in
          let tree = Option.value_exn file.typed_tree in
          Typecheck.typecheck_module
            ~verbose prog
            ~import_checker:(import_checker env m file)
            tree
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
  let rec compile_file_path ~config entry_file_path : profile list =
    let { build_dir; runtime_dir; platform; verbose; wasm_standalone; _ } = config in
    try
      (* ctx is a typing context for all modules *)
      let prog = Lichenscript_typing.Program.create ~reverse_symbol:false () in
      let env = create ~prog ~config () in

      (* parse the entry dir *)
      let dir_of_entry = Filename.dirname entry_file_path in
      let entry_full_path = parse_module_by_dir ~prog env ~real_path:(FS.get_realpath dir_of_entry) dir_of_entry  in

      typecheck_all_modules ~prog ~verbose env;

      let main_mod = Option.value_exn (Linker.get_module env.linker (Option.value_exn entry_full_path)) in
      if List.is_empty (Module.files main_mod) then (
        printf "No files should be compiled.\n";
        ignore (exit 0)
      );
      let file = List.hd_exn (Module.files main_mod) in
      
      let typed_tree: Typedtree.program = Option.value_exn Module.(file.typed_tree) in

      let main_fun_id = typed_tree.tprogram_scope#find_type_symbol "main" in
      if Option.is_none main_fun_id then (
        printf "\"main\" function is not found, nothing to output.\n";
        ignore (exit 0)
      );

      let declarations = Linker.link_from_entry env.linker ~verbose (Option.value_exn main_fun_id) in

      let get_build_dir () =
        match build_dir with
        | Some v -> v
        | None -> (
          let working_dir = Sys.getcwd() in
          let tmp = Filename.concat working_dir ".lichenscript" in
          if not (FS.is_directory tmp) then (
            FS.mkdir_p tmp
          );
          tmp
        )
      in

      let external_resources = Linker.external_resources env.linker in

      match platform with
      | "native"
      | "wasm32" -> (
        let include_dir_names = Hash_set.create (module String) in
        let c_assets = ref [] in

        let includes: string list =
          List.filter_map
            ~f:(fun path ->
              let _, ext_opt = Filename.split_extension path in
              match ext_opt with
              | Some "h" -> (
                let dirname, filename = Filename.split path in
                Hash_set.add include_dir_names dirname;
                Some filename
              )
              | Some "c" -> (
                c_assets := (path::(!c_assets));
                None
              )
              | _ -> None
            )
            external_resources
        in

        let ptr_size =
          match platform with
          | "wasm32" -> Lichenscript_ir.Ir.Ptr32
          | _ -> Lichenscript_ir.Ir.Ptr64
        in

        let output = Lichenscript_c.codegen
          ~prog
          ~includes
          ~init_calls:env.before_eval_fun_call
          ~ptr_size
          declarations
        in
        let mod_name = entry_file_path |> Filename.dirname |> last_piece_of_path in
        let build_dir = get_build_dir () in
        let output_path = write_to_file build_dir mod_name ~ext:".c" output in
        let bin_name = entry_file_path |> last_piece_of_path |> (Filename.chop_extension) in

        let ext_includes = Hash_set.to_list include_dir_names in

        let c_assets =
          !c_assets
          |> List.rev
          |> List.map
            ~f:(fun asset ->
              let filename = Filename.basename asset in
              let prefix, _ = Filename.split_extension filename in
              ("ext_" ^ prefix), prefix, asset
            )
        in

        write_makefiles
          ~bin_name
          ~runtime_dir
          ~platform
          ~wasm_standalone
          ~ext_includes
          build_dir
          ((mod_name, mod_name, output_path)::c_assets)
      )

      | "js" -> (

        let preclude_ext =
          List.fold
            ~init:""
            ~f:(fun acc path ->
              let _, ext_opt = Filename.split_extension path in
              match ext_opt with
              | Some "js" -> (
                let file_content = FS.read_file_content path in
                acc ^ "\n" ^ file_content
              )

              | _ -> acc
            )
            external_resources
        in

        let js_runtime_preclude_file = Filename.(concat (concat runtime_dir "js") "runtime.js") in
        let preclude_runtime = FS.read_file_content js_runtime_preclude_file in
        let preclude = preclude_runtime ^ "\n" ^ preclude_ext in
        let output = Lichenscript_js.codegen ~prog ~preclude ~init_calls:env.before_eval_fun_call declarations in
        let mod_name = entry_file_path |> Filename.dirname |> last_piece_of_path in
        let build_dir = get_build_dir () in
        let output_path = write_to_file build_dir mod_name ~ext:".js" output in
        [{
          profile_name = "release";
          profile_dir = build_dir;
          profile_exe_path = output_path;
        }]
      )

      | _ -> failwith ("unknown platform: " ^ platform)
    
    with
      | Diagnosis.Error e ->
        raise (TypeCheckError [e])

  and write_to_file build_dir mod_name ~ext content: string =
    if (not (FS.file_exists build_dir)) then (
      FS.mkdir_p build_dir
    );
    let output_file_path = Filename.concat build_dir (mod_name ^ ext) in
    if String.equal ext ".c" then (
      write_file_content_if_changed output_file_path ~data:content
    ) else (
      FS.write_file_content output_file_path ~data:content
    );
    output_file_path

  and write_makefiles ~bin_name ~runtime_dir ~platform ~wasm_standalone ~ext_includes build_dir mods: profile list =
    let debug_dir =
      match platform with
      | "native" -> Filename.concat build_dir "debug"
      | "wasm32" -> Filename.concat build_dir "wasm32_debug"
      | _ -> failwith ("unsupport platform: " ^ platform)
    in
    let release_dir =
      match platform with
      | "native" ->Filename.concat build_dir "release"
      | "wasm32" -> Filename.concat build_dir "wasm32_release"
      | _ -> failwith  ("unsupport platform: " ^ platform)
    in
    let bin_name =
      match (platform, wasm_standalone) with
      | ("native", _) -> bin_name
      | ("wasm32", false) -> bin_name ^ ".js"
      | ("wasm32", true) -> bin_name ^ ".wasm"
      | _ -> failwith  ("unsupport platform: " ^ platform)
    in
    FS.mkdir_p debug_dir;
    FS.mkdir_p release_dir;
    write_makefiles_with_mode ~bin_name ~runtime_dir ~mode:"debug" ~platform ~ext_includes debug_dir mods;
    write_makefiles_with_mode ~bin_name ~runtime_dir ~mode:"release" ~platform ~ext_includes release_dir mods;
    [
      {
        profile_name = "debug";
        profile_dir = debug_dir;
        profile_exe_path = Filename.concat debug_dir bin_name;
      };
      {
        profile_name = "release";
        profile_dir = release_dir;
        profile_exe_path = Filename.concat release_dir bin_name;
      }
    ]

  and write_makefiles_with_mode ~bin_name ~runtime_dir ~mode ~platform ~ext_includes build_dir mods =
    let output_path = Filename.concat build_dir "Makefile" in
    let open Makefile in
    let runtime_dir = Filename.concat runtime_dir "c" in
    let c_srcs = List.fold ~init:"runtime.o" ~f:(fun acc (_, m, _) -> (acc ^ " " ^ m ^ ".o")) mods in

    let ext_flags =
      List.fold
        ~init:""
        ~f:(fun acc ic ->
          acc ^ " -I" ^ ic
        )
        ext_includes
    in
    
    let entries = List.concat [
      [
        {
          entry_name = "all";
          deps = List.concat [ ["runtime"]; (List.map ~f:(fun (m, _, _) -> m) mods)];
          content = [
            Format.sprintf "$(CC) $(FLAGS) %s -o %s" c_srcs bin_name
          ];
        };
        {
          entry_name = "runtime";
          deps =
            List.map
            ~f:(fun name ->
              (Filename.concat runtime_dir name)
              |> FS.get_realpath
            )
            ["runtime.c"; "runtime.h"];
          content = [
            "$(CC) $(FLAGS) -c " ^ Filename.(concat runtime_dir "runtime.c"|> FS.get_realpath)
          ];
        }
      ];
      List.map
        ~f:(fun (m, _, output) ->
          let output_full_path = FS.get_realpath output in
          {
            entry_name = m;
            deps = [output_full_path];
            content = [
              Format.sprintf "$(CC) $(FLAGS) -I%s%s -c %s" (FS.get_realpath runtime_dir) ext_flags output_full_path
            ];
          }
        )
        mods;
    ] in
    let flags =
      if String.equal mode "debug" then
        "FLAGS=-O0 -g3 -D LSC_DEBUG\n"
      else
        "FLAGS=-O3 -g0\n"
    in

    let cc =
      match platform with
      | "native" -> "cc"
      | "wasm32" -> "emcc"
      | _ -> failwith  ("unsupport platform: " ^ platform)
    in
    let data =
      "CC=" ^ cc ^ "\n" ^
      flags ^
      to_string entries in
    write_file_content_if_changed output_path ~data

  and write_file_content_if_changed output_path ~data =
    let digest = Md5.digest_string data in
    let digest_str = Md5.to_hex digest in
    let hash_path = output_path ^ ".md5" in

    let default() =
      FS.write_file_content output_path ~data;
      FS.write_file_content hash_path ~data:digest_str
    in

    if FS.is_file hash_path then (
      let hash_content = FS.read_file_content hash_path in
      if not (String.equal digest_str hash_content) then (
        default()
      )
    ) else
      default()
  
end
