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
open Lichenscript_common.Cli_utils

let help_message = {|
|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
file_packer <dir> [<args>]

|} ^ TermColor.bold ^ "Options:" ^ TermColor.reset ^ {|
  -o  Output path
  -h  Show help message

|}

type file_content =
  | File of string
  | Dir of string list

type file_item = {
  file_path: string;
  file_content: file_content;
}

let rec collect_item base_dir path =
  if Sys.is_file_exn path then (
    let file_content = In_channel.read_all path in
    [{
      file_path = path;
      file_content = File file_content;
    }]
  ) else if Sys.is_directory_exn path then (
    let children =
      Sys.ls_dir path
      |> List.map ~f:(Filename.concat path)
    in
    let dir_item = {
      file_path = path;
      file_content = Dir children;
    } in

    let children_items =
      children
      |> List.map ~f:(collect_item base_dir)
      |> List.concat
    in

    dir_item::children_items
  ) else
    []

let rec main () =
  let args = Sys.get_argv () in
  let index = ref 2 in
  let output = ref None in
  let base = ref None in
  let traverse_dir = Array.get args 1 in
  while !index < Array.length args do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-o" -> (
      let next = Array.get args !index in
      output := Some next;
      index := !index + 1;
    )

    | "--base" -> (
      let next = Array.get args !index in
      base := Some next;
      index := !index + 1;
    )

    | "-h" | "--help" ->
      Format.printf "%s\n" help_message;
      ignore (exit 0)

    | _ ->
      Format.printf "unknown option: %s\n" item;
      ignore (exit 2)

  done;
  match !base with
  | Some base_dir ->
    build_ml traverse_dir base_dir !output

  | None ->
    Format.printf "no base dir\n";
    ignore (exit 2)

and build_ml traverse_dir base_dir output =
  let buf = Buffer.create 1024 in

  let base_dir = Filename.realpath base_dir in
  let items = collect_item base_dir traverse_dir in

  let normalize_path path =
    let realpath = Filename.realpath path in
    String.slice realpath (String.length base_dir) (String.length realpath)
  in

  Buffer.add_string buf ("let contents  = [");
  List.iter
    ~f:(fun item ->
      Buffer.add_string buf ("  (\"" ^ (normalize_path item.file_path) ^ "\", ");
      (match item.file_content with
      | File content -> (
        Buffer.add_string buf "`File {|";
        Buffer.add_string buf content;
        Buffer.add_string buf "|}"
      )
      | Dir children ->
        Buffer.add_string buf "`Dir [\n";
        List.iter
          ~f:(fun path_content ->
            Buffer.add_string buf "    \"";
            let real_base = Filename.realpath item.file_path in
            let realpath = Filename.realpath path_content in
            let relactive = 
              String.slice realpath (String.length real_base) (String.length realpath)
            in
            Buffer.add_string buf relactive;
            Buffer.add_string buf "\";\n";
          )
          children;
        Buffer.add_string buf "  ]";
      );
      Buffer.add_string buf ");\n";
    )
    items;
  Buffer.add_string buf "]";

  Out_channel.write_all (Option.value_exn output) ~data:(Buffer.contents buf)

;;
main ()
