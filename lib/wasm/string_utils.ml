open Core_kernel

let u8string_to_u16string (str: string) =
  let bytes = str
    |> String.to_array
    |> Array.map ~f:Char.to_int
  in
  let hex = Array.map ~f:(fun cp -> Printf.sprintf "\\%02x" cp) bytes in
  String.concat_array hex
