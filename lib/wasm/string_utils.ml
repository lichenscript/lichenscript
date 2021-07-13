open Core_kernel

let u8string_to_buffer (str: string): Buffer.t =
  let buffer = Buffer.create 0 in
  String.iter
    ~f:(fun ch ->
      (* check little-endian *)
      Buffer.add_char buffer ch;
      Buffer.add_char buffer (Char.unsafe_of_int 0)
    )
    str;
  Buffer.add_char buffer (Char.unsafe_of_int 0);
  Buffer.add_char buffer (Char.unsafe_of_int 0);
  buffer
