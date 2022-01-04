open Cli_utils
open Core

let help_message = {|

|} ^ TermColor.bold ^ "Usage:" ^ TermColor.reset ^ {|
wtc <entry> [args]

|} ^ TermColor.bold ^ "Options:" ^ TermColor.reset ^ {|
  -h, --help        Show help message

|}

let print_error_prefix () =
  Out_channel.print_string (TermColor.red ^ "Error: " ^ TermColor.reset)

let rec main () = 
  let index = ref 1 in
  let args = Sys.get_argv () in
  let entry = ref None in
  while !index < (Array.length args) do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-h" | "--help" ->
      Format.printf "%s" help_message;
      ignore (exit 0)

    | _ ->
      entry := Some item

  done;
  if Option.is_none !entry then (
    Format.printf "Error: no input files\n";
    ignore (exit 1)
  ) else 
    build_entry (Option.value_exn !entry)

and build_entry (entry_dir: string) =
  let toml_filename = Filename.concat entry_dir "water.toml" in
  (match Sys.file_exists toml_filename with 
  | `No -> (
    print_error_prefix ();
    Out_channel.printf "water.toml is not found in %s\n" (TermColor.grey ^ entry_dir ^ TermColor.reset);
    ignore (exit 1)
  )
  | _ -> ()
  );
  let toml_content = In_channel.read_all toml_filename in
  let parse_result = Toml.Parser.from_string toml_content in
  match parse_result with
  | `Ok data ->
    build_with_toml entry_dir data

  | `Error (msg, location) -> (
    let open Toml.Parser in
    Out_channel.printf "Error occurs while parsing water.toml:\n";
    let error_msg = Format.sprintf "%d:%d %s" location.line location.column msg in
    Out_channel.print_endline (TermColor.grey ^ error_msg ^ TermColor.reset)
  )

and build_with_toml entry_dir toml_data =
  let open Toml.Types in
  try
    let package = Toml.Types.Table.find (Toml.Min.key "package") toml_data in
    let package_name =
      match package with
      | TTable pkg_table  ->  (
        let name = Toml.Types.Table.find (Toml.Min.key "name") pkg_table in
        match name with
        | TString s -> s
        | _ -> (
          print_error_prefix ();
          Out_channel.printf "\"package.name\" is not a string\n";
          ignore (exit 1);
          failwith "unrechable"
        )
      )
      | _ -> (
        print_error_prefix ();
        Out_channel.printf "\"package.name\" is a table\n" ;
        ignore (exit 1);
        failwith "unrechable"
      )
    in
    let tasks = extract_tasks toml_data in
    build_package ~package_name ~entry_dir tasks
  with
    | Stdlib.Not_found -> (
      print_error_prefix ();
      Out_channel.printf "\"package\" is not found in toml\n" ;
      (ignore (exit 1))
    )

and extract_tasks table : Task.t list =
  let open Toml.Types in
  let result = ref [] in
  (try
    let bin = Toml.Types.Table.find (Toml.Min.key "bin") table in
    match bin with
    | TArray (NodeTable tbl_list) ->
      List.iter
        ~f:(fun tbl -> (
          let task_name =
            match Toml.Types.Table.find (Toml.Min.key "name") tbl with
            | TString str -> str
            | _ -> failwith "can not find name"
          in
          let entry_file =
            match Toml.Types.Table.find (Toml.Min.key "path") tbl with
            | TString str -> str
            | _ -> failwith "can not find path"
          in
          result := {
            Task.
            task_name;
            entry_file;
          }::!result
        ))
        tbl_list

    | _ -> ()
  with
    (* TODO: friendly error message *)
    | _ -> ()
  );
  List.rev !result

and build_package ~package_name ~entry_dir tasks =
  List.iter ~f:(Resolver.build_task ~package_name ~entry_dir) tasks

;;

main()
