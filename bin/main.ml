open Lichenscript_lex
open Lichenscript_parsing
open Lichenscript_typing
open Lichenscript_resolver
open Lichenscript_common.Cli_utils
open Core

let help_message = {|
|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
wtc <command> [<args>]

|} ^ TermColor.bold ^ "Subcommands" ^ TermColor.reset ^ {|
  build
  run

|}

let build_help_message = {|
|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
wtc build <entry> [<args>]

|} ^ TermColor.bold ^ "Options:" ^ TermColor.reset ^ {|
  --lib                 Build a library
  --base <dir>          Base directory to resolve modules,
                        default: current directory
  --build-dir, -D <dir> Specify a directory to build,
                        a temp directory will be used if this is not specified.
  --runtime, -R <dir>   The directory of runtime.
  --std <dir>           Specify the directorey of std library.
                        If this is not passed, use the builtin version.
  --platform <platform> native/wasm/js, default: native
  --debug               Enable debug message
  -h, --help            Show help message

|}

let rec main () = 
  let index = ref 1 in
  let args = Sys.get_argv () in
  if Array.length args < 2 then (
    Format.print_string help_message;
    ignore (exit 1)
  );
  let command = Array.get args !index in
  index := !index + 1;
  match command with
  | "build" -> ignore (build_command args index)
  | "run" -> build_and_run args index
  | _ -> (
    Format.printf "unkown command %s\n" command;
    ignore (exit 1)
  )

and build_and_run args index =
  let build_result = build_command args index in
  match build_result with
  | Some path ->
    run_bin_path path

  | None -> ()

and build_command args index : string option =
  if !index >= (Array.length args) then (
    Format.printf "%s" build_help_message;
    ignore (exit 1);
    None
  ) else 
    let entry = ref None in
    let std = ref None in
    let buildDir = ref None in
    let runtimeDir = ref None in
    let debug = ref false in
    let platform = ref "native" in
    let baseDir = ref Filename.current_dir_name in
    while !index < (Array.length args) do
      let item = Array.get args !index in
      index := !index + 1;
      match item with
      | "-h" | "--help" ->
        Format.printf "%s" build_help_message;
        ignore (exit 0)

      | "--debug" ->
        debug := true

      | "--std" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --std\n";
          ignore (exit 1)
        );
        std := Some (Array.get args !index);
        index := !index + 1;
      )

      | "--build-dir" | "-D" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for %s\n" item;
          ignore (exit 1)
        );
        buildDir := Some (Array.get args !index);
        index := !index + 1;
      )

      | "--runtime" | "-R" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for %s\n" item;
          ignore (exit 1)
        );
        runtimeDir := Some (Array.get args !index);
        index := !index + 1
      )

      | "--platform" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --platform\n";
          ignore (exit 1)
        );
        platform := (Array.get args !index);
        index := !index + 1;
      )

      | "--base" -> (
        if !index >= (Array.length args) then (
          Format.printf "not enough args for --base\n";
          ignore (exit 1)
        );
        baseDir := (Array.get args !index) |> Filename.realpath;
        index := !index + 1;
      )

      | _ ->
        entry := Some item

    done;
    if Option.is_none !entry then (
      Format.printf "Error: no input files\n";
      ignore (exit 1);
      None
    ) else 
      build_entry (Option.value_exn !entry) !std !buildDir !runtimeDir !debug

and build_entry (entry: string) std_dir build_dir runtime_dir debug: string option =
  let open Resolver in
  if Option.is_none std_dir then (
    Format.printf "std library is not found\n";
    ignore (exit 1)
  );
  if Option.is_none runtime_dir then (
    Format.printf "runtime library is not found\n";
    ignore (exit 1)
  );
  try
    let entry_full_path = Filename.realpath entry in
    let build_dir, result = Resolver.compile_file_path ~std_dir ~build_dir ~runtime_dir ~debug entry_full_path in
    run_make_in_dir build_dir;
    result
  with
    | Unix.Unix_error (_, err, err_s) -> (
      Format.printf "Failed: %s: %s %s\n" entry err err_s;
      None
    )

    | TypeCheckError errors ->
      List.iter
        ~f:(fun err ->
          let { Type_error. spec; loc; ctx } = err in
          print_loc_title ~prefix:"type error" loc;
          let start = loc.start in
          Format.printf "%d:%d %a\n" start.line start.column (Type_error.PP.error_spec ~ctx) spec
        )
        errors;
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
      None

and run_make_in_dir build_dir =
  Out_channel.printf "Spawn to build in %s\n" (TermColor.bold ^ build_dir ^ TermColor.reset);
  Out_channel.flush Out_channel.stdout;
  Out_channel.flush Out_channel.stderr;
  match Unix.fork () with
  | `In_the_child -> 
    Unix.chdir build_dir;
    Unix.exec ~prog:"make" ~argv:["make";] () |> ignore

  | `In_the_parent pid ->
    ignore (Unix.waitpid pid)

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

and run_bin_path path =
  match Unix.fork () with
  | `In_the_child -> (
    ignore (Unix.exec ~prog:path ~argv:[path] ())
  )
  | `In_the_parent pid -> (
    Unix.waitpid_exn pid
  )

;;

main()
