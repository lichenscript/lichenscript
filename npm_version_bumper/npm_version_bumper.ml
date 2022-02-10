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
A tool to bump version of npm.

|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
npm_verion_bumper <dir> --main <main_dir>

|} ^ TermColor.bold ^ "Options:" ^ TermColor.reset ^ {|
  --main <dir>  Name of main repo dir
  -h, --help    Show help message

|}

let version_regex = Re.Pcre.regexp ~flags:[`MULTILINE] "\\\"version\\\":\\s+\\\"([^\"]+)\\\""

let rec main () =
  let index = ref 1 in
  let args = Sys.get_argv () in
  let main_repo = ref "" in
  let dir = ref "" in
  while !index < (Array.length args) do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-h" | "--help" ->
      Format.printf "%s" help_message;
      ignore (exit 0)

    | "--main" -> (
      let next_item = Array.get args !index in
      main_repo := next_item;
      index := !index + 1
    )

    | _ ->
      dir := item

  done;

  if (String.is_empty !main_repo) || (String.is_empty !dir) then (
    print_endline help_message
  ) else
    bump_version !dir !main_repo

and bump_version dir main_repo_name =
  let main_repo_package_json_path = dir ^ "/" ^ main_repo_name ^ "/package.json" in
  let main_repo_package_json_content = In_channel.read_all main_repo_package_json_path in
  let test_result = Re.exec version_regex main_repo_package_json_content in
  let groups = Re.Group.all test_result in
  if Array.length groups < 2 then (
    Format.eprintf "can not find version in %s" main_repo_package_json_path;
    ignore (exit 1)
  ) else (
    let prev_version = Array.get groups 1 in
    Out_channel.printf "previous version is %s, please input new version: " (TermColor.green ^ prev_version ^ TermColor.reset);
    Out_channel.(flush stdout);
    let new_version = In_channel.(input_line_exn stdin) in
    replace_version_in_file main_repo_package_json_path prev_version new_version;
    replace_all_dir_in_dir ~except:main_repo_name dir prev_version new_version
  )

and replace_all_dir_in_dir ~except dir old_version version =
  let dirs = Sys.readdir dir in
  Array.iter
    ~f:(fun dir_item ->
      if String.(dir_item <> except) then (
        let joined_file = dir ^ "/" ^dir_item ^ "/package.json" in
        match Sys.is_file joined_file with
        | `Yes -> (
          replace_version_in_file joined_file old_version version
        )

        | _ -> ()

      )
    )
    dirs

and replace_version_in_file filename old_version version =
  let content = In_channel.read_all filename in
  let content = String.substr_replace_all content ~pattern:("\"" ^ old_version ^ "\"") ~with_:("\"" ^ version ^ "\"") in
  Out_channel.write_all filename ~data:content;
  Out_channel.printf "Written: %s\n" (TermColor.grey ^ filename ^ TermColor.reset)

;;

main ()
