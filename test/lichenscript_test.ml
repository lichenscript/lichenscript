(*
 * This file is part of LichenScript Compiler.
 *
 * LichenScript Compiler is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * LichenScript Compiler is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with LichenScript Compiler. If not, see <https://www.gnu.org/licenses/>.
 *)
open Core
open Lichenscript_common.Cli_utils

type test_suite = {
  mutable stdout:        Core.Unix.File_descr.t option;
  pid:           Pid.t;
  test_file:     string;
  stdout_buffer: Buffer.t;
}

type t = {
  mutable totoal_files: int;
  mutable finished_files: int;
  mutable error_files: int;
  rest_args: string array;
  compiler_path: string;
  (* fd -> suites *)
  suites: (int, test_suite) Hashtbl.t;
}

let create ~rest_args ~compiler_path () = {
  totoal_files = 0;
  finished_files = 0;
  error_files = 0;
  rest_args;
  compiler_path;
  suites = Hashtbl.create (module Int);
}

let help_message = {|
Test suite for LichenScript, usage:

lichencscript_test <dirs> -- <rest>

  -N, --name        Only run the test matched the name
  -C, --compiler    The path of the compiler

|}

let rec main () =
  let index = ref 1 in
  let args = Sys.get_argv () in
  let compiler_path = ref "" in
  let suite_name = ref None in
  let rest_args = ref [||] in

  let test_dir = Array.get args !index in
  index := !index + 1;

  while !index < (Array.length args) do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-C" | "--compiler" ->
      begin
        if !index >= (Array.length args) then (
          Format.printf "not enough args for %s\n" item;
          ignore (exit 1)
        );
        compiler_path := (Array.get args !index); 
        index := !index + 1
      end

    | "-N" | "--name" ->
      begin
        if !index >= (Array.length args) then (
          Format.printf "not enough args for %s\n" item;
          ignore (exit 1)
        );
        suite_name := Some (Array.get args !index);
        index := !index + 1
      end

    | "--" -> (
      let length = Array.length args in
      rest_args := Array.slice args !index length;
      index := length
    )

    | _ ->
      Format.eprintf "unknown option: %s\n" item;
      ignore (exit 1);

  done;
  let env = create  ~compiler_path:!compiler_path ~rest_args:!rest_args () in
  let full_path = Filename.realpath test_dir in
  run_test_in_dir env ?name:!suite_name full_path;
  receive_all_messages env

and remove_suite_by_fd env fd =
  let fd = Unix.File_descr.to_int fd in
  let suite = Hashtbl.find_exn env.suites fd in
  suite.stdout <- None

and receive_all_messages env =
  if env.totoal_files = 0 then (
    print_statistics env;
    ignore (exit 0)
  );

  (* read til the nd *)
  let handle_message fd =
    let fd_int = Unix.File_descr.to_int fd in
    try
      let suite = Hashtbl.find_exn env.suites fd_int in
      let content_bytes = Bytes.make 1024 (Char.of_int_exn 0) in
      let read_bytes = Unix.read ~len:1024 ~buf:content_bytes fd in

      if read_bytes = 0 then (
        remove_suite_by_fd env fd
      ) else
        Buffer.add_subbytes suite.stdout_buffer content_bytes ~pos:0 ~len:read_bytes
    with exn -> 
      match exn with
      | Stdlib.End_of_file ->
        remove_suite_by_fd env fd

      | _->
        Format.eprintf "unexpected error finished: %s, len of fd: %d\n" (Exn.to_string exn) (Hashtbl.length env.suites);
        remove_suite_by_fd env fd

  in

  let input_channels =
    env.suites
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (_, suite) -> Option.to_list suite.stdout)
    |> List.concat
  in

  if not (List.is_empty input_channels) then (
    let { Unix.Select_fds.read; _ } = Unix.select ~read:input_channels ~write:[] ~except:[] ~timeout:`Never () in
    List.iter ~f:handle_message read;
    receive_all_messages env
  ) else (
    print_statistics env;
    ignore (exit 0)
  )

and print_statistics env =
  Hashtbl.iter
    ~f:(fun suite ->
      let _exit = Unix.waitpid suite.pid in
      match _exit with
      | Ok _ ->
        diff_stdout env suite

      | Error (`Signal _ex) -> (
        Format.printf "%sFailed test:%s %s: \n" TermColor.red TermColor.reset suite.test_file;
        Format.printf "%s" (Buffer.contents suite.stdout_buffer);
        env.error_files <- env.error_files + 1
      )
      | Error (`Exit_non_zero code) -> (
        handle_negative_exit env suite code
      )
    )
    env.suites;
  if env.error_files = 0 then
    Format.printf "%s[DONE]%s " TermColor.green TermColor.reset
  else
    Format.printf "%s[FAILED]%s " TermColor.green TermColor.reset
  ;
  Format.printf "Totally: %d, finished: %d, failed: %d\n" env.totoal_files env.finished_files env.error_files

and handle_negative_exit env suite code =
  let dirname = Filename.dirname suite.test_file in
  let error_file = Filename.concat dirname "error.txt" in
  if Sys.is_file_exn error_file then (
    let error_content = In_channel.read_all error_file in
    let stdout_content = Buffer.contents suite.stdout_buffer in
    let stdout_lines = String.split ~on:'\n' stdout_content in
    let found_expected =
      List.fold
        ~init:false
        ~f:(fun acc line ->
          if acc then acc
          else
            String.equal line error_content
        )
        stdout_lines
    in
    if found_expected then (
      Format.printf "%s[TEST]%s %s\n" TermColor.green TermColor.reset suite.test_file;
      env.finished_files <- env.finished_files + 1
    ) else (
      Format.printf "%s[ERROR]%s %s\n" TermColor.red TermColor.reset suite.test_file;
      Format.printf "Expected error: %s\n" error_content;
    )
  ) else (
    Format.printf "%sFailed Test:%s %s, code: %d: \n" TermColor.red TermColor.reset suite.test_file code;
    Format.printf "%s" (Buffer.contents suite.stdout_buffer);
    env.error_files <- env.error_files + 1
  )

and diff_stdout env suite =
  let std_out_content = Buffer.contents suite.stdout_buffer in

  let dirname = Filename.dirname suite.test_file in
  let expect_file = Filename.concat dirname "expect.txt" in
  let error_file = Filename.concat dirname "error.txt" in

  if Sys.is_file_exn expect_file then (
    let expect_file_content = In_channel.read_all expect_file in
    if String.equal expect_file_content std_out_content then (
      Format.printf "%s[TEST]%s %s\n" TermColor.green TermColor.reset suite.test_file;
      env.finished_files <- env.finished_files + 1
    ) else (
      Format.printf "%s[ERROR]%s %s\n" TermColor.red TermColor.reset suite.test_file;
      Format.printf "Expect:\n%s" expect_file_content;
      Format.printf "Actual:\n%s" std_out_content;
      env.error_files <- env.error_files + 1
    )
  ) else (
    if Sys.is_file_exn error_file then (
      Format.printf "%s[ERROR]%s %s\n" TermColor.red TermColor.reset suite.test_file;
      Format.printf "Expected to failed, but succeed\n";
      env.error_files <- env.error_files + 1
    ) else
      Format.printf "%s[TEST]%s %s\n" TermColor.green TermColor.reset suite.test_file;
      Format.printf "%s" std_out_content;
      env.finished_files <- env.finished_files + 1
  )


(*
 * Traverse the test_dir, findout all the .lc files
 *)
and run_test_in_dir env ?name test_dir =
  let filenames = Sys.ls_dir test_dir in
  List.iter
    ~f:(fun filename ->
      let child_path = Filename.concat test_dir filename in
      if Sys.is_directory_exn child_path then
        run_test_in_dir ?name env child_path
      else if (String.equal filename "main.lc") then (
        let child_path_parts = Filename.parts test_dir in
        let last_slice = List.last_exn child_path_parts in
        match name with
        | Some name ->
          if String.equal last_slice name then
            run_test_for_file env child_path
          else ()

        | _ ->
          run_test_for_file env child_path

      ) else ()
    )
    filenames

and run_test_for_file env test_file =
  env.totoal_files <- env.totoal_files + 1;

  let dirname = Filename.dirname test_file in
  let dirname_slices = Filename.parts dirname in
  let dirname = List.last_exn dirname_slices in
  let test_dir = Filename.concat "_test" dirname in
  Unix.mkdir_p test_dir;

  let args = List.concat [
    [ env.compiler_path; "run"; test_file ];
    Array.to_list env.rest_args;
    [ "-D"; test_dir ]
  ] in

  Out_channel.printf "Run test: %s\n" test_file;
  Out_channel.flush Out_channel.stdout;

  let pipe_read, pipe_write = Unix.pipe () in
  match Unix.fork() with
  | `In_the_child -> (
    Unix.dup2 ~src:pipe_write ~dst:Unix.stdout ();

    Unix.close pipe_read;
    Unix.close pipe_write;

    let _ = Unix.exec ~prog:env.compiler_path ~argv:args () in
    failwith "unreachable"
  )
  | `In_the_parent pid -> (
    Unix.close pipe_write;

    let suite = {
      pid;
      stdout = Some pipe_read;
      test_file;
      stdout_buffer = Buffer.create 1024;
    } in
    let fd = Unix.File_descr.to_int pipe_read in
    Hashtbl.set env.suites ~key:fd ~data:suite
  )

let () =
  main()
