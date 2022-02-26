open Core_kernel

type entry = {
  entry_name: string;
  deps: string list;
  content: string list;
}

let to_string (entries: entry list) =
  let buf = Buffer.create 1024 in
  Buffer.add_string buf "\n";
  List.iter
    ~f:(fun entry ->
      let { entry_name; deps; content } = entry in
      Buffer.add_string buf entry_name;
      Buffer.add_string buf ": ";
      List.iter
        ~f:(fun dep ->
          Buffer.add_string buf dep;
          Buffer.add_string buf " "
        )
        deps;
      Buffer.add_string buf "\n";
      Buffer.add_string buf "\t";
      List.iter
        ~f:(fun line ->
          Buffer.add_string buf line;
          Buffer.add_string buf "\n";
        )
        content;
    )
    entries;
  Buffer.contents buf
