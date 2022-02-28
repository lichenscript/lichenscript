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

let message = {|
js_output_wrapper <file>

This is a tool to wrap the output of esreverse.
Because js_of_ocaml uses some Node.js module, which can not be built by esbuild.

|}

let polyfill = {|

function node_require_fs() {
  return {};
}

function node_require_constants() {
  return {};
}

function node_require_tty() {
  return {};
}

function node_require_child_process() {
  return {};
}

|}

let require_fs = Re.Pcre.regexp ~flags:[`MULTILINE] {|require\("fs"\)|}
let require_constants = Re.Pcre.regexp {|require\("constants"\)|}
let require_tty = Re.Pcre.regexp {|require\("tty"\)|}
let require_child_process = Re.Pcre.regexp {|require\("child_process"\)|}
let global_return_this = Re.Pcre.regexp {|function\(\)\{return this\}\(\)|}

let read_file_and_print filename =
  let old_content = In_channel.read_all filename in

  let content =
    old_content
    |> Re.replace_string ~all:true require_fs ~by:"node_require_fs()"
    |> Re.replace_string ~all:true require_constants ~by:"node_require_constants()"
    |> Re.replace_string ~all:true require_tty ~by:"node_require_tty()"
    |> Re.replace_string ~all:true require_child_process ~by:"node_require_child_process()"
    |> Re.replace_string ~all:true global_return_this ~by:"window"
  in

  Format.print_string (polyfill ^ content)

let () =
  let args = Sys.get_argv () in
  if Array.length args < 2 then (
    Format.print_string message
  ) else (
    read_file_and_print (Array.get args 1)
  )