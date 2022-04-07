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
open Js_of_ocaml
open Core_kernel
open Lichenscript_lex
open Lichenscript_parsing
open Lichenscript_typing
open Lichenscript_resolver

module AstMap = Hashtbl.Make(String)

exception Abort

let js_find_path config =
  let find_path_array = (Js.Unsafe.coerce config)##.findPaths |> Js.to_array in
  find_path_array
  |> Array.map ~f:Js.to_string
  |> Array.to_list

let get_keywords_completions_by_scope (scope_type: Scope.scope_type): Auto_complete.CompletionItem.t array =
  let open Scope in
  let keywords =
    match scope_type with
    | ST_Class _ -> [|
      "static";
      "protected";
      "public";
      "private";
      "virtual";
      "declare";
    |]
    | ST_Root
    | ST_Module
    | ST_File ->
      [|
        "import";
        "interface";
        "function";
        "class";
        "enum";
        "public";
        "declare";
      |]
    | ST_Normal
    | ST_Function
    | ST_While
    | ST_Lambda ->
      [|
        "let"; "const";
        "while"; "for"; "if";
        "in";
        "match"; "case";
      |]
  in
  Array.map
    ~f:(fun keyword -> { Auto_complete.CompletionItem.
      label = keyword;
      kind = Auto_complete.CompletionItemKind.Keyword;
      detail = None;
    })
    keywords

let create dummy_fs js_config =
  let module FS = struct
    let is_directory (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##isDirectory (Js.string path) in
      Js.to_bool (Js.Unsafe.coerce ret)

    let is_file (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##isFile (Js.string path) in
      Js.to_bool (Js.Unsafe.coerce ret)

    let get_realpath (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##getRealPath (Js.string path) in
      Js.to_string (Js.Unsafe.coerce ret)

    let ls_dir (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##lsDir (Js.string path) in
      let ret_arr = Js.to_array (Js.Unsafe.coerce ret) in
      ret_arr
      |> Array.to_list
      |> List.map ~f:Js.to_string

    let mkdir_p (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##mkdirRecursive (Js.string path) in
      ignore ret

    let file_exists (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##fileExists (Js.string path) in
      Js.to_bool (Js.Unsafe.coerce ret)

    let read_file_content (path: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##readFileContent (Js.string path) in
      Js.to_string (Js.Unsafe.coerce ret)

    let write_file_content (path: string) ~(data: string) =
      let ret = (Js.Unsafe.coerce dummy_fs)##writeFileContent (Js.string path) (Js.string data) in
      ignore ret

  end in

  let module R = Resolver.S(FS) in

  let runtime_dir = (Js.Unsafe.coerce js_config)##.runtimeDir |> Js.to_string in

  let config = { R.
    find_paths = js_find_path js_config;
    build_dir = None;
    runtime_dir;
    platform = "native";
    verbose = false;
    wasm_standalone = false;
  } in

  let prog = ref (Program.create ~reverse_symbol:true ()) in

  let resolver = ref (R.create ~prog:!prog ~config ()) in

  let ast_map = AstMap.create () in

  let rec annotate_module_by_dir ~diagnostics dir _first_source =
    let iterate_file mod_path =
      let open R in
      let module_scope = new module_scope ~prev:(Program.root_scope !prog) () in

      let preclude_path = (Js.Unsafe.coerce js_config)##.precludeDir |> Js.to_string in
      let is_std = String.is_substring_at dir ~pos:0 ~substring:preclude_path in
      let _mod = Module.create ~full_path:mod_path ~is_std ~module_scope () in
      Linker.set_module (!resolver).linker mod_path _mod;
      let children = FS.ls_dir mod_path in
      List.iter
        ~f:(fun item ->
          let child_path = Filename.concat mod_path item in
          if FS.is_file child_path then (
            try[@alert "-deprecated"]  (* disable the deprecated alert *)
              let test_result = Re.exec allow_suffix child_path |> Re.Group.all in
              if Array.length test_result > 1 then ((* is a .lc file *)
                annotate_file ~diagnostics ~mod_path:dir _mod child_path
                (* compile_file_to_path ~prog ~mod_path env _mod child_path *)
              )
            with
            | Not_found -> ()
          ) else ()
        )
        children;
      Module.finalize_module_exports _mod;
    in
    if not (Linker.has_module (!resolver).linker dir) then (
      iterate_file dir
    ) else ()

  and annotate_file ~diagnostics ~mod_path _mod path =

    (* compile_file_to_path in resolver.ml *)
    let annotate_internall path ast =
      let imports = R.preclude_std_for_imports ast in

      let import_star_external_modules = ref [] in
      let imports_map = Hashtbl.create (module String) in

      let find_path = R.resolve_import_path !resolver ~mod_path in

      let handle_import_lc_module import spec =
        let open Ast.Import in
        let { source; source_loc; _ } = import in
        let result = find_path source in
        match result with
        | Some (path, source) -> (
          ignore (annotate_module_by_dir ~diagnostics path source);
          Hashtbl.set imports_map ~key:source ~data:path;
          match spec with
          | ImportAll ->
            import_star_external_modules := path::!import_star_external_modules;
          | _ -> ()
        )
        | None -> (
          let diagnostic = Js_helper.mk_diagnostic
            ~loc:source_loc
            Js_helper.Error
            ("can not resolve " ^ source)
          in
          diagnostics := diagnostic::(!diagnostics);
          raise Abort
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
          if not (FS.file_exists normalized_path) then (
            let diagnostic = Js_helper.mk_diagnostic
              ~loc:source_loc
              Js_helper.Error
              ("file not found: " ^ source)
            in
            diagnostics := diagnostic::(!diagnostics);
          )
        )

        | Some _ -> (
          let diagnostic = Js_helper.mk_diagnostic
            ~loc:source_loc
            Js_helper.Error
            ("can not resolve: " ^ source)
          in
          diagnostics := diagnostic::(!diagnostics);
        )
        | None -> (
          let diagnostic = Js_helper.mk_diagnostic
            ~loc:source_loc
            Js_helper.Error
            ("unknown extension: " ^ source)
          in
          diagnostics := diagnostic::(!diagnostics);
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
      let file_scope = new R.file_scope ~prev:module_scope !resolver import_star_external_modules in
      (* parse and create env, do annotation when all files are parsed
      * because annotation stage needs all exported symbols are resolved
      *)
      let typed_env = Lichenscript_typing.Env.create
        ~file_scope
        ~external_resolver:(R.external_resolver !resolver imports_map)
        ~allow_external:true
        !prog
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
      R.insert_moudule_file !resolver ~mod_path file

    in
    let ast_opt = AstMap.find ast_map path in
    match ast_opt with
    | Some ast -> (
      annotate_internall path ast
    )
    | None -> (
      let content = FS.read_file_content path in
      let ast_result = parse_and_cache path content in
      match ast_result with
      | Result.Ok ast ->
        annotate_internall path ast
      | Result.Error errors -> (
        let js_errors =
          errors
          |> List.map ~f:(Utils.parse_error_to_diagnostic)
          |> List.rev
        in
        diagnostics := List.append js_errors !(diagnostics) ;
        raise Abort
      )

    )

  and parse_and_cache path content =
    let file_key = File_key.LibFile path in
    Parser.parse_string (Some file_key) content

  in

  object%js

    method parseAndCache path content =
      let path = Js.to_string path in
      let file_content = Js.to_string content in
      let ast_result = parse_and_cache path file_content in
      match ast_result with
      | Result.Ok ast -> (
        AstMap.set ast_map ~key:path ~data:ast;
        new%js Js.array_empty
      )

      | Result.Error raw_errors ->
        Utils.parse_errors_to_js_array raw_errors

    method typecheckDir dir =
      let result = ref [] in

      prog := Program.create ~reverse_symbol:true ();
      resolver := R.create ~prog:!prog ~config ();

      (try
        let dir = Js.to_string dir in
        annotate_module_by_dir ~diagnostics:result dir dir;
        R.typecheck_all_modules ~prog:!prog ~verbose:false !resolver;
      with
      | Diagnosis.Error e -> (
        let ser =
          match e.spec with
          | Diagnosis.Dg_error _ -> Js_helper.Error
          | Diagnosis.Dg_warning _ -> Js_helper.Warning
        in
        let content =
          match e.spec with
          | Diagnosis.Dg_error err ->
            Format.asprintf "%a" (Diagnosis.PP.error_spec ~ctx:e.ctx) err
          | Diagnosis.Dg_warning _ ->
            "Warning"
        in
        let js_diagnostic = Js_helper.mk_diagnostic ~loc:(e.loc) ser content in
        result := js_diagnostic::(!result);
      )

      | Abort -> ());

      !result
      |> List.rev 
      |> List.to_array
      |> Js.array

    method findDefinition path offset =
      let path = Js.to_string path in
      let reverse_map = Option.value_exn (!prog).reverse_symbol in
      let result = ReverseSymbol.find_symbol reverse_map path offset in
      match result with
      | Some ty_var -> (
        let node = Program.get_node !prog ty_var in
        let node_loc = node.loc in
        let path = Format.asprintf "%a" File_key.pp (Option.value_exn node_loc.source) in
        let definition =
          object%js

            val uri = Js.string ("file://" ^ path)

            val range = Js_helper.mk_range node_loc

          end
        in
        Js.def definition
      )

      | None -> Js.undefined

    method findCompletion path offset =
      let path = Js.to_string path in
      let reverse_map = Option.value_exn (!prog).reverse_symbol in
      let scope_opt = ReverseSymbol.find_scope_in_range reverse_map path offset in
      match scope_opt with
      | Some scope -> (
        let open Core_type in
        let open Auto_complete in
        let scope_type = scope#scope_type in
        Format.printf "find completion in scope: %a\n" Scope.pp_scope_type scope_type;
        let keywords = get_keywords_completions_by_scope scope_type in
        let vars = scope#vars_to_root in
        vars
        |> List.to_array
        |> Array.map
          ~f:(fun (name, var) ->
            let open Scope in
            let ty_expr = Program.deref_node_type !prog var.var_id in
            let kind =
              match ty_expr with
              | TypeExpr.TypeDef { TypeDef. spec = TypeDef.Enum _ ; _ } ->
                CompletionItemKind.Enum
              | TypeExpr.TypeDef { TypeDef. spec = TypeDef.Class _ ; _ } ->
                CompletionItemKind.Class
              | TypeExpr.TypeDef { TypeDef. spec = TypeDef.Interface _ ; _ } ->
                CompletionItemKind.Interface
              | TypeExpr.TypeDef { TypeDef. spec = TypeDef.ClassMethod _ ; _ } ->
                CompletionItemKind.Method
              | TypeExpr.TypeDef { TypeDef. spec = TypeDef.Function _ ; _ } ->
                CompletionItemKind.Function
              | _ ->
                CompletionItemKind.Variable;
            in
            let detail = Program.print_type_value !prog ty_expr in
            { CompletionItem.
              label = name;
              kind;
              detail = Some detail;
            }
          )
        |> Array.append keywords
        |> Array.map ~f:Js_helper.completion_item_to_js
        |> Js.array
      )

      | _ ->
        new%js Js.array_empty

    method deleteFile path =
      AstMap.remove ast_map path

  end
