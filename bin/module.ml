open Core
open Waterlang_typing

type t = {
  id: string array;
  id_str: string;
  source: string;
  typed_tree: Typedtree.program;
}

let get_id_str id =
  let buf = Buffer.create 64 in
  let len = Array.length id in
  let index = ref 0 in
  while !index < len do
    Buffer.add_string buf (Array.get id !index);
    if !index < (len - 1) then (
      Buffer.add_string buf "."
    );
    index := !index + 1;
  done;
  Buffer.contents buf

let create ~path ~id ~id_str typed_tree =
  {
    id;
    id_str;
    source = path;
    typed_tree;
  }
