
open Core
open Lichenscript_common.Cli_utils

(* let temp_dir_name = "/tmp/test_waterlang/"

let test_parser _ =
  let result =  Parser.parse_string None "

  @external(\"env\", \"console_log\")
  declare function print(content: string);

  class Array {

    private __name: sring;

    public static New() {

    }

  }

  enum Result {
    Ok,
    Error,
  }

  @export
  function main(args, args2: string, ...rest) {
    let name = 3;
    name = 4;
  }
  " in
  match result with
  | Result.Ok _program ->
    (* Ast.pp_program Format.std_formatter program; *)
    (* let _env = Lichenscript_typing.Env.create () in *)
    (* let program = Lichenscript_typing.Annotate.annotate env program in
    Lichenscript_typing.Typecheck.type_check env program; *)
    assert true

  | Result.Error errs ->
    errs
    |> List.rev
    |> List.iter
       (fun error ->
         let str = Format.asprintf "%a" Parse_error.PP.error error in
         let { Loc. line; column; } = error.perr_loc.start in
         Format.printf "%d:%d %s\n" line column str;
        );
    assert false

let test_codegen _ =
  let source = "
    function main(a: i32, b: i32): i32 {
      return a + b;
    }
    "
  in
  let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result

let test_codegen_binary _ =
  let source = "
    @export
    function main(a: i32, b: i32): i32 {
      return a + b;
    }
    "
  in
  let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result

let test_type_checking _ =
  let source = "
    function main(a: i32, b: i32): f32 {
      return a + b;
    }
    "
  in
  assert_raises
    (Utils.ExpectedError "Error: 3:17 Type 'i32' can not be returned because 'f32' is expected")
    (fun _ ->
      Utils.parse_string_and_codegen source
    )

let test_function_call _ =
  let source = "
    function add(a: i32, b: i32): i32 {
      return a + b;
    }

    function main(a: i32, b: i32): i32 {
      return add(a, b);
    }
    "
  in
  let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result

let test_string _ =
  let source = "
    @external(\"env\", \"console_log\")
    declare function print(content: string)

    function main() {
      let a: string = \"Hello World!\";
      print(a);
    }
  "
  in
  let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result;
  ()

  (* Core.Unix.mkdir_p temp_dir_name;
  let test_output_name = temp_dir_name ^ "test_wtl" in
  Format.printf "output name: %s" test_output_name;
  let _result = Utils.parse_string_and_codegen source in
  let in_chan = Core.Unix.open_process_in ("node " ^ test_output_name ^ ".js" ) in
  let r = Core.In_channel.input_all in_chan in
  Core.In_channel.close in_chan;
  assert_equal r "Hello World!\n" *)

let test_assignment _ =
  let source = "
    @external(\"env\", \"console_log\")
    declare function print(content: string)

    function main() {
      let name: string = \"Hi\";
      let counter: i32 = 0;
      while 1 {
        if counter > 10 {
          break;
        } else {
          print(name);
          counter = counter + 1;
        }
      }
    }
  "
  in
  (* let result = Utils.parse_string_and_codegen source in
  Format.printf "%s" result *)
  Core.Unix.mkdir_p temp_dir_name;
  let test_output_name = temp_dir_name ^ "test_wtl_2" in
  Format.printf "output name: %s" test_output_name;
  let _result = Utils.parse_string_and_codegen source in
  let in_chan = Core.Unix.open_process_in ("node " ^ test_output_name ^ ".js" ) in
  let r = Core.In_channel.input_all in_chan in
  Core.In_channel.close in_chan;
  print_string r

let suite =
  "TestParser" >::: [
    "test_parser" >:: test_parser;
    "test_codegen" >:: test_codegen;
    "test_codegen_binary" >:: test_codegen_binary;
    "test_type_checking" >:: test_type_checking;
    (* "test_function_call" >:: test_function_call;
    "test_string" >:: test_string; *)

    (* "test_assignment" >:: test_assignment; *)
  ] *)

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

  -C, --compiler    The path of the compiler

|}

let rec main () =
  let index = ref 1 in
  let args = Sys.get_argv () in
  let compiler_path = ref "" in
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
  run_test_in_dir env full_path;
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
      | Ok _ -> (
        Format.printf "Test: %s: \n" suite.test_file;
        Format.printf "%s" (Buffer.contents suite.stdout_buffer);
        env.finished_files <- env.finished_files + 1
      )
      | Error (`Signal _ex) -> (
        Format.printf "%sFailed test:%s %s: \n" TermColor.red TermColor.reset suite.test_file;
        Format.printf "%s" (Buffer.contents suite.stdout_buffer);
        env.error_files <- env.error_files + 1
      )
      | Error (`Exit_non_zero code) -> (
        Format.printf "%sFailed Test:%s %s, code: %d: \n" TermColor.red TermColor.reset suite.test_file code;
        Format.printf "%s" (Buffer.contents suite.stdout_buffer);
        env.error_files <- env.error_files + 1
      )
    )
    env.suites;
  Format.printf "Totally: %d, finished: %d, failed: %d\n" env.totoal_files env.finished_files env.error_files

(*
 * Traverse the test_dir, findout all the .lc files
 *)
and run_test_in_dir env test_dir =
  let filenames = Sys.ls_dir test_dir in
  List.iter
    ~f:(fun filename ->
      let child_path = Filename.concat test_dir filename in
      if Sys.is_directory_exn child_path then
        run_test_in_dir env child_path
      else if (String.equal filename "main.lc") then (
        run_test_for_file env child_path
      ) else ()
    )
    filenames

and run_test_for_file env test_file =
  env.totoal_files <- env.totoal_files + 1;

  let args = List.concat [
    [ env.compiler_path; "run"; test_file ];
    [ "--" ];
    Array.to_list env.rest_args;
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
