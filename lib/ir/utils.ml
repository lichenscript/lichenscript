open Core_kernel

let c0 = Char.to_int '0'
let c9 = Char.to_int '9'

let ca = Char.to_int 'a'

let cz = Char.to_int 'z'

let cA = Char.to_int 'A'

let cZ = Char.to_int 'z'

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
      else (
        if ch < ' ' then (
          Buffer.add_string buffer "\\";
          Buffer.add_string buffer (Int.to_string (Char.to_int ch));
        ) else
          Buffer.add_char buffer ch
      )
    )
    content;
  Buffer.contents buffer
