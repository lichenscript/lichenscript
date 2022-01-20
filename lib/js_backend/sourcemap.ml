open Core_kernel

let base64_encoding_table = [|
  'A'; 'B'; 'C'; 'D'; 'E'; 'F'; 'G'; 'H';
  'I'; 'J'; 'K'; 'L'; 'M'; 'N'; 'O'; 'P';
  'Q'; 'R'; 'S'; 'T'; 'U'; 'V'; 'W'; 'X';
  'Y'; 'Z'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f';
  'g'; 'h'; 'i'; 'j'; 'k'; 'l'; 'm'; 'n';
  'o'; 'p'; 'q'; 'r'; 's'; 't'; 'u'; 'v';
  'w'; 'x'; 'y'; 'z'; '0'; '1'; '2'; '3';
  '4'; '5'; '6'; '7'; '8'; '9'; '+'; '/';
|]

let int_to_base64 code =
  Array.get base64_encoding_table code

let int_to_vlq buffer code =
  let s1 = ref (
    if code < 0 then
      ((((-1) * code) lsl 1) lor 1)
    else
      code lsl 1
  ) in
  if !s1 > 0b11111 then (
    let local_buffer = Array.create ~len:8 0 in
    let counter = ref 0 in
    while !s1 > 0b11111 do
      Array.set local_buffer !counter (!s1 land 0b11111);
      counter := !counter + 1;
      s1 := !s1 lsr 5;
    done;
    Array.set local_buffer !counter !s1;
    counter := !counter + 1;

    for i = 0 to (!counter - 1) do
      if i <> !counter - 1 then (
        let tmp = Array.get local_buffer i in
        Array.set local_buffer i (tmp lor 0b100000)
      );
      let ch = int_to_base64 (Array.get local_buffer i) in
      Buffer.add_char buffer ch
    done
  ) else  (
    let ch = int_to_base64 !s1 in
    Buffer.add_char buffer ch
  )

let generate_vlq_str buffer transformed_column file_index before_line before_column =
  int_to_vlq buffer transformed_column;
  int_to_vlq buffer file_index;
  int_to_vlq buffer before_line;
  int_to_vlq buffer before_column

let sw _new old = _new - old

class sourcemap_generator = object

  val mutable _line_counter = 1
  val mutable _after_col = 0
  val mutable _file_index = 0
  val mutable _before_line = 1
  val mutable _before_col = 0

  val buffer = Buffer.create 1024

  method add_location after_col file_id before_line before_col =
    if file_id < 0 then ()
    else (
      generate_vlq_str
        buffer
        (sw after_col _after_col)
        (sw file_id _file_index)
        (sw before_line _before_line)
        (sw before_col _before_col);
      
      _after_col <- after_col;
      _file_index <- file_id;
      _before_line <- before_line;
      _before_col <- before_col;
    )
      

end
