open Core_kernel

let escape_string content =
  let buffer = Buffer.create (String.length content) in
  String.iter
    ~f:(fun ch ->
      let open Char in
      if ch = '\\' then
        Buffer.add_string buffer "\\\\"
      else if ch = '"' then
        Buffer.add_string buffer "\\\""
      else if ch = '\n' then
        Buffer.add_string buffer "\\n"
      else if ch = '\r' then
        Buffer.add_string buffer "\\r"
      else if ch = '\t' then
        Buffer.add_string buffer "\\t"
      else
        Buffer.add_char buffer ch
    )
    content;
  Buffer.contents buffer
