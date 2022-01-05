open Cli_utils
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
  --std <dir>           Specify the directorey of std library.
                        If this is not passed, use the builtin version.
  --platform <platform> native/wasm/js, default: native
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
  | "build" -> build_command args index
  | _ -> (
    Format.printf "unkown command %s\n" command;
    ignore (exit 1)
  )

and build_command args index =
  if !index >= (Array.length args) then (
    Format.printf "%s" build_help_message;
    ignore (exit 1)
  );
  let entry = ref None in
  let std = ref None in
  let buildDir = ref None in
  let platform = ref "native" in
  let baseDir = ref Filename.current_dir_name in
  while !index < (Array.length args) do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-h" | "--help" ->
      Format.printf "%s" build_help_message;
      ignore (exit 0)

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
        Format.printf "not enough args for --build-dir\n";
        ignore (exit 1)
      );
      buildDir := Some (Array.get args !index);
      index := !index + 1;
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
    ignore (exit 1)
  ) else 
    build_entry (Option.value_exn !entry) !std !buildDir

and build_entry (entry: string) std_dir build_dir =
  Resolver.compile_file_path ~std_dir ~build_dir entry

;;

main()
