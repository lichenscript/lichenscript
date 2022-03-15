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

let js_find_path config =
  let find_path_array = (Js.Unsafe.coerce config)##.findPaths |> Js.to_array in
  find_path_array
  |> Array.map ~f:Js.to_string
  |> Array.to_list

let create dummy_fs config =
  let module R = Resolver.S(
    struct

      let is_directory path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##isDirectory in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        Js.to_bool (Js.Unsafe.coerce ret)

      let is_file path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##isFile in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        Js.to_bool (Js.Unsafe.coerce ret)

      let get_realpath path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##getRealPath in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        Js.to_string (Js.Unsafe.coerce ret)

      let ls_dir path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##lsDir in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        let ret_arr = Js.to_array (Js.Unsafe.coerce ret) in
        ret_arr
        |> Array.to_list
        |> List.map ~f:Js.to_string

      let mkdir_p path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##mkdirRecursive in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        ignore ret

      let file_exists path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##fileExists in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        Js.to_bool (Js.Unsafe.coerce ret)

      let read_file_content path =
        let _fun = (Js.Unsafe.coerce dummy_fs)##readFileContent in
        let ret = Js.Unsafe.call _fun Js.undefined [| Js.Unsafe.coerce (Js.string path); |] in
        Js.to_string (Js.Unsafe.coerce ret)

      let write_file_content path ~data =
        let _fun = (Js.Unsafe.coerce dummy_fs)##writeFileContent in
        let ret = Js.Unsafe.call _fun Js.undefined [|
          Js.Unsafe.coerce (Js.string path);
          Js.Unsafe.coerce (Js.string data);
        |] in
        ignore ret
    
    end
  ) in

  let runtime_dir = (Js.Unsafe.coerce config)##.runtimeDir |> Js.to_string in

  let config = { R.
    find_paths = js_find_path config;
    build_dir = None;
    runtime_dir;
    platform = "native";
    verbose = false;
    wasm_standalone = false;
  } in

  let prog = Program.create () in

  let _resolver = R.create ~prog ~config () in

  let ast_map = AstMap.create () in
  object%js

    method parseAndCacheWillThrow path content =
      let path = Js.to_string path in
      let file_content = Js.to_string content in
      let file_key = File_key.LibFile path in
      let ast_result = Parser.parse_string (Some file_key) file_content in
      match ast_result with
      | Result.Ok ast -> (
        AstMap.set ast_map ~key:path ~data:ast
      )

      | Result.Error raw_errors -> (
        let err = Utils.parse_errors_to_js_error raw_errors in
        Js_error.raise_ err
      )

    method deleteFile path =
      AstMap.remove ast_map path

  end
