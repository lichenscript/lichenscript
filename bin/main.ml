open Cli_utils
open Core

let help_message = {|

|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
wtc <entry> [args]

|} ^ TermColor.bold ^ "Options:" ^ TermColor.reset ^ {|
  --lib             Build a library
  --std <dir>       Specific the directorey of std library
  -h, --help        Show help message

|}

let rec main () = 
  let index = ref 1 in
  let args = Sys.get_argv () in
  let entry = ref None in
  let std = ref None in
  while !index < (Array.length args) do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-h" | "--help" ->
      Format.printf "%s" help_message;
      ignore (exit 0)

    | "--std" -> (
      if !index >= (Array.length args) then (
        Format.printf "not enough args for --std\n";
        ignore (exit 1)
      );
      std := Some (Array.get args !index);
      index := !index + 1;
    )

    | _ ->
      entry := Some item

  done;
  if Option.is_none !entry then (
    Format.printf "Error: no input files\n";
    ignore (exit 1)
  ) else 
    build_entry (Option.value_exn !entry) !std

and build_entry (entry: string) std_dir =
  let abs_path = Filename.realpath entry in
  let package_name = Filename.dirname abs_path in
  Resolver.compile_file_path ~package_name ~std_dir entry

;;

main()
