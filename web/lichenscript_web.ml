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
open Lichenscript_resolver
open Lichenscript_resolver.Resolver
open Js_of_ocaml

type fs_item =
| FS_file of string
| FS_dir of string list

module FileMap = Hashtbl.Make(String)

let global_fs: fs_item FileMap.t = FileMap.create ();

module JsFS : Resolver.FSProvider = struct

  let is_directory path =
    match FileMap.find global_fs path with
    | Some (FS_dir _) -> true
    | _ -> false

  let is_file path =
    match FileMap.find global_fs path with
    | Some (FS_file _) -> true
    | _ -> false

  let get_realpath path = path

  let ls_dir path =
    match FileMap.find global_fs path with
    | Some (FS_dir paths) -> paths
    | _ -> []

  let mkdir_p path =
    match FileMap.find global_fs path with
    | None ->
      FileMap.set global_fs ~key:path ~data:(FS_dir [])
    | _ -> ()

  let file_exists path =
    match FileMap.find global_fs path with
    | None -> false
    | Some _ -> true

  let read_file_content path =
    match FileMap.find global_fs path with
    | Some (FS_file content) -> content
    | _ -> failwith ("file not found: " ^ path)

  let write_file_content path ~data =
    FileMap.set global_fs ~key:path ~data:(FS_file data)
  
end

let _ =
  let adder (path, item) =
      match item with
      | `File file ->
        Hashtbl.set global_fs ~key:path ~data:(FS_file file)
      
      | `Dir content ->
        Hashtbl.set global_fs ~key:path ~data:(FS_dir content)
  in

  List.iter ~f:adder Runtime.contents;
  List.iter ~f:adder Preclude.contents;

  Js.export_all
    (object%js

    method createIntellisenseInstance = Intellisense.create()

    method compile str =
      let module R = Resolver.S (JsFS) in
      try
        let dummy_path = "/usr/admin/main.lc" in
        let oc_string = Js.to_string str in
        JsFS.write_file_content dummy_path ~data:oc_string;

        Hashtbl.set
          global_fs
          ~key:"/usr/admin"
          ~data:(FS_dir [
            "main.lc";
          ]);

        let config = { R.
          find_paths = ["/std"];
          runtime_dir = "/runtime";
          build_dir = Some "/usr/build";
          platform = "js";
          verbose = false;
          wasm_standalone = false;
        } in
        let profiles = R.compile_file_path ~config dummy_path
        in
        let profile = List.hd_exn profiles in
        let profile_path = profile.profile_exe_path in
        let js_content = JsFS.read_file_content profile_path in
        Js.string js_content
      with
      | ResolveError err -> (
        let err_content = Format.asprintf "%a" Resolve_error.pp_spec err.spec in

        let err_obj = object%js
          val line = err.loc.start.line
          val column = err.loc.start.column
          val source =
            match err.loc.source with
            | Some source ->
              let source_str = Format.asprintf "%a" Lichenscript_lex.File_key.pp source in
              Js.string source_str
            | None -> Js.string ""
          val content = Js.string err_content

        end in

        let error_list = Js.array [| err_obj |] in

        let js_err = new%js Js.error_constr (Js.string "TypeCheckError") in
        Js.Unsafe.set js_err (Js.string "errors") error_list;
        Js_error.raise_ (Js_error.of_error js_err)
      )

      | TypeCheckError raw_errors -> (
        let open Lichenscript_typing in
        let error_list = Js.array [||] in

        List.iteri
          ~f:(fun index err ->
            let err_content =
              match err.spec with
              | Diagnosis.Dg_error dg_err ->
                Format.asprintf "%a" (Diagnosis.PP.error_spec ~ctx:err.ctx) dg_err
              | _ -> ""
            in
            let err_obj = object%js
              val line = err.loc.start.line
              val column = err.loc.start.column
              val source =
                match err.loc.source with
                | Some source ->
                  let source_str = Format.asprintf "%a" Lichenscript_lex.File_key.pp source in
                  Js.string source_str
                | None -> Js.string ""
              val content = Js.string err_content

            end in
            Js.array_set error_list index err_obj
          )
          raw_errors;

        let js_err = new%js Js.error_constr (Js.string "TypeCheckError") in
        Js.Unsafe.set js_err (Js.string "errors") error_list;
        Js_error.raise_ (Js_error.of_error js_err)
      )

      | ParseError raw_errors -> (
        let err = Intellisense.parse_errors_to_js_error raw_errors in
        Js_error.raise_ err
      )

      | e ->
        let msg = (Exn.to_string e) |> Js.string in
        let js_err = new%js Js.error_constr msg in
        Js_error.raise_ (Js_error.of_error js_err)

    end)
