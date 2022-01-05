open Core
open Waterlang_typing

type file = {
	path: string;
  typed_tree: Typedtree.program;
}

type t = {
  id: string array;
  id_str: string;
  mutable files: file list;
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

let create ~id ~id_str () =
  {
    id;
    id_str;
    files = [];
  }

let add_file env file =
  env.files <- file::env.files

let files env = List.rev env.files

let set_files env files =
  env.files <- files
