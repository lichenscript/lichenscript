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

module AstMap = Hashtbl.Make(String)

let parse_errors_to_js_error raw_errors =
  let error_list = Js.array [||] in

  List.iteri
    ~f:(fun index err ->
      let err_content = Format.asprintf "%a" Lichenscript_parsing.Parse_error.PP.error err in
      let err_obj = object%js
        val line = err.perr_loc.start.line
        val column = err.perr_loc.start.column
        val source =
          match err.perr_loc.source with
          | Some source ->
            let source_str = Format.asprintf "%a" Lichenscript_lex.File_key.pp source in
            Js.string source_str
          | None -> Js.string ""
        val content = Js.string err_content

      end in
      Js.array_set error_list index err_obj
    )
    raw_errors;

  let js_err = new%js Js.error_constr (Js.string "ParseError") in
  Js.Unsafe.set js_err (Js.string "errors") error_list;
  Js_error.of_error js_err

let create () =
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
        let err = parse_errors_to_js_error raw_errors in
        Js_error.raise_ err
      )

    method deleteFile path =
      AstMap.remove ast_map path

  end
