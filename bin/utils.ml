open Core

let read_all_into_buffer pipe =
  let buffer = Buffer.create 1024 in

  let rec handle_message fd =
    try
      let content_bytes = Bytes.make 1024 (Char.of_int_exn 0) in
      let read_bytes = Unix.read ~len:1024 ~buf:content_bytes fd in

      if read_bytes = 0 then (
        ()
      ) else (
        Buffer.add_subbytes buffer content_bytes ~pos:0 ~len:read_bytes;
        handle_message fd
      )
    with exn -> 
      match exn with
      | Stdlib.End_of_file ->
        ()

      | _->
        ()

  in

  handle_message pipe;

  Buffer.contents buffer
