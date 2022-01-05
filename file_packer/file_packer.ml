open Core

let rec main () =
  let args = Sys.get_argv () in
  let index = ref 1 in
  let output = ref None in
  let inputs = ref [] in
  while !index < Array.length args do
    let item = Array.get args !index in
    index := !index + 1;
    match item with
    | "-o" -> (
      let next = Array.get args !index in
      output := Some next;
      index := !index + 1;
    )
    | _ -> inputs := item::!inputs
  done;
  build_ml !inputs !output

and build_ml inputs output =
  let buf = Buffer.create 1024 in

  Buffer.add_string buf ("let contents  = [");
  List.iter
    ~f:(fun input ->
      let content = In_channel.read_all input in
      let filename = Filename.basename input in
      Buffer.add_string buf ("(\"" ^ filename ^ "\", ");
      Buffer.add_string buf "{|";
      Buffer.add_string buf content;
      Buffer.add_string buf "|} );\n";
    )
    inputs;
  Buffer.add_string buf "]";

  Out_channel.write_all (Option.value_exn output) ~data:(Buffer.contents buf)

;;
main ()
