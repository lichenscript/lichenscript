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
open Lichenscript_lex
open Lichenscript_parsing
open Lichenscript_typing
open Lichenscript_resolver
open Lichenscript_resolver.Resolver
open Lichenscript_common.Cli_utils
open Core

let help_message = {|
|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
lsc <command> [<args>]

|} ^ TermColor.bold ^ "Subcommands" ^ TermColor.reset ^ {|
  build
  run

|}

let build_help_message = {|
|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
lsc build <entry> [<args>]

|} ^ TermColor.bold ^ "Options:" ^ TermColor.reset ^ {|
  --lib                        Build a library
  --base <dir>                 Base directory to resolve modules,
                               default: current directory
  --build-dir, -D <dir>        Specify a directory to build,
                               a temp directory will be used if this is not specified.
  --platform <platform>        native/wasm32/js, default: native
  --mode <debug|release>       Choose the mode of debug/release
  --verbose, -V                Print verbose log
  --search-paths               Show the search paths
  --standalone-wasm <executor> Build the standalone wasm, specify the executor
  -h, --help                   Show help message

|} ^ TermColor.bold ^ "Environment:" ^ TermColor.reset ^ {|
LSC_RUNTIME              The directory of runtime.
LSC_STD                  Specify the directorey of std library.

|}

type build_result = {
  build_exe: string;
  build_executor: string option;
}

let rec main () = 
  let index = ref 1 in
  let args = Sys.get_argv () in
  if Array.length args < 2 then (
    Format.print_string help_message;
    ignore (exit 2)
  );
  let command = Array.get args !index in
  index := !index + 1;
  match command with
  | "build" -> ignore (build_command args index)
  | "run" -> build_and_run args index
  | _ -> (
    Format.printf "unkown command %s\n" command;
    ignore (exit 2)
  )

and build_and_run args index =
  let build_result = build_command args index in
  match build_result with
  | Some path ->
    ignore (run_bin_path path)

  | None -> ()

and build_command args index : build_result option =
  if !index >= (Array.length args) then (
    Format.printf "%s" build_help_message;
    ignore (exit 2);
    None
  ) else 
    let entry = ref None in
    let buildDir = ref None in
    let wasm_standalone = ref None in
    let mode = ref "debug" in
    let verbose = ref false in
    let platform = ref "native" in
    let baseDir = ref Filename.current_dir_name in
    while !index < (Array.length args) do
      let item = Array.get args !index in
      index := !index + 1;
      match item with
      | "-h" | "--help" ->
        Format.printf "%s" build_help_message;
        ignore (exit 0)

      | "--mode" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --mode\n";
          ignore (exit 2)
        );
        mode := Array.get args !index;
        index := !index + 1;
        if (not (String.equal !mode  "debug")) && (not (String.equal !mode "release")) then (
          Format.printf "mode should be debug or release\n";
          ignore (exit 2)
        )
      )

      | "-V" | "--verbose" ->
        verbose := true

      | "--search-paths" -> (
        let current_dir = Unix.getcwd () in
        let paths = Search_path.get_search_path_from_node current_dir in
        List.iter
          ~f:(fun path -> printf "%s\n" path)
          paths;
        ignore (exit 0)
      )

      | "--build-dir" | "-D" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for %s\n" item;
          ignore (exit 2)
        );
        buildDir := Some (Array.get args !index);
        index := !index + 1;
      )

      | "--platform" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --platform\n";
          ignore (exit 2)
        );
        platform := (Array.get args !index);
        index := !index + 1;
      )

      | "--standalone-wasm" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --standalone-wasm\n";
          ignore (exit 2)
        );
        wasm_standalone := Some (Array.get args !index);
        index := !index + 1;
      )

      | "--base" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --base\n";
          ignore (exit 2)
        );
        baseDir := (Array.get args !index) |> Filename.realpath;
        index := !index + 1;
      )

      | _ ->
        entry := Some item

    done;
    if Option.is_none !entry then (
      Format.printf "Error: no input files\n";
      ignore (exit 2);
      None
    ) else (
      let std = Sys.getenv_exn "LSC_STD" in
      let runtimeDir = Sys.getenv_exn "LSC_RUNTIME" in
      build_entry (Option.value_exn !entry) std !buildDir runtimeDir !mode !verbose !platform !wasm_standalone
    )

and build_entry (entry: string) std_dir build_dir runtime_dir mode verbose platform wasm_standalone: build_result option =
  let module R = Resolver.S (struct

    let is_directory path = Sys.is_directory_exn path

    let is_file path = Sys.is_file_exn path

    let get_realpath = Filename.realpath

    let ls_dir = Sys.ls_dir

    let mkdir_p path = Unix.mkdir_p path

    let file_exists path = Sys.file_exists_exn path

    let read_file_content = In_channel.read_all

    let write_file_content = Out_channel.write_all

  end) in
  let open R in
  try
    let entry_full_path = Filename.realpath entry in
    let entry_dir = Filename.dirname entry_full_path in
    let find_paths = List.append
      [Filename.realpath std_dir]
      (Search_path.get_search_path_from_node entry_dir)
    in
    let config = {
      find_paths;
      build_dir;
      runtime_dir;
      verbose;
      platform;
      wasm_standalone = Option.is_some wasm_standalone;
    } in
    let profiles = compile_file_path ~config entry_full_path in
    let profile =
      if String.equal platform "js" then
        List.hd_exn profiles
      else
        List.find_exn
          ~f:(fun profile ->
            String.equal profile.profile_name mode
          )
          profiles
    in
    if not (String.equal platform "js") then (
      run_make_in_dir profile.profile_dir
    );
    Some {
      build_exe = profile.profile_exe_path;
      build_executor =
        if String.equal platform "js" then
          Some "node"
        else
          wasm_standalone;
    }
  with
    | Unix.Unix_error (_, err, err_s) -> (
      Format.printf "Failed: %s: %s %s\n" entry err err_s;
      ignore (exit 2);
      None
    )

    | ResolveError err ->
      print_loc_title ~prefix:"resolve error" err.loc;
      let start = err.loc.start in
      Format.printf "%d:%d %a\n" start.line start.column Resolve_error.pp_spec err.spec;
      ignore (exit 2);
      None

    | TypeCheckError errors ->
      let open Diagnosis in
      List.iter
        ~f:(fun err ->
          let { spec; loc; ctx } = err in
          print_loc_title ~prefix:"type error" loc;
          let start = loc.start in
          match spec with
          | Dg_error err_spec -> (
            Format.printf "%d:%d %a\n" start.line start.column (Diagnosis.PP.error_spec ~ctx) err_spec
          )
          | Dg_warning _ -> ()

        )
        errors;
      ignore (exit 2);
      None

    | ParseError errors ->
      List.iter
        ~f:(fun err ->
          let { Parse_error. perr_loc; _ } = err in
          print_loc_title ~prefix:"parse error" perr_loc;
          let start = perr_loc.start in
          Format.printf "%d:%d %a\n" start.line start.column Parse_error.PP.error err
        )
        errors;
      ignore (exit 2);
      None

    | Module.ReexportSymbol(loc, prev, new_export) ->
      print_loc_title ~prefix:"resolve error" loc;
      let start = loc.start in
      Format.printf "%d:%d Re-export symbol '%s', previous definition is in: %s\n"
        start.line start.column new_export.export_name (loc_to_string prev.export_var.var_loc);
      ignore (exit 2);
      None

and run_make_in_dir build_dir =
  (* Out_channel.printf "Spawn to build in %s\n" (TermColor.bold ^ build_dir ^ TermColor.reset); *)
  Out_channel.flush Out_channel.stdout;
  Out_channel.flush Out_channel.stderr;

  let pipe_read, pipe_write = Unix.pipe () in
  match Unix.fork () with
  | `In_the_child -> 
    Unix.dup2 ~src:pipe_write ~dst:Unix.stdout ();

    Unix.close pipe_read;
    Unix.close pipe_write;

    Unix.chdir build_dir;
    Unix.exec ~prog:"make" ~argv:["make";] () |> ignore

  | `In_the_parent pid -> (
    Unix.close pipe_write;

    let std_out_content = Utils.read_all_into_buffer pipe_read in

    let result = Unix.waitpid pid in

    match result with
    | Ok _ -> ()
    | Error _ -> Out_channel.print_string std_out_content
  )

and print_loc_title ~prefix loc_opt =
  Loc. (
    match loc_opt.source with
    | Some source -> (
      print_error_prefix ();
      let source_str = Format.asprintf "%a" Lichenscript_lex.File_key.pp source in
      Out_channel.printf "%s in %s\n" prefix (TermColor.bold ^ source_str ^ TermColor.reset)
    )
    | None -> ()
  )

and loc_to_string (loc_opt: Loc.t) =
  Loc.(
    match loc_opt.source with
    | Some source -> (
      print_error_prefix ();
      let source_str = Format.asprintf "%a:%d:%d" Lichenscript_lex.File_key.pp source loc_opt.start.line loc_opt.start.column in
      source_str
    )
    | None -> ""
  )

(* replace current process *)
and run_bin_path build_result =
  let { build_exe; build_executor } = build_result in
  match build_executor with
  | Some executor ->
    Unix.exec ~prog:executor ~argv:[executor; build_exe] ()

  | None -> Unix.exec ~prog:build_exe ~argv:[build_exe] ()

;;

main()
