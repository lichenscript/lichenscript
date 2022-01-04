open Core
open Waterlang_typing

type t = {
  name: string;
  source: string;
  typed_tree: Typedtree.program;
}

let create path typed_tree =
  let dirname = Filename.dirname path in
  {
    name = dirname;
    source = path;
    typed_tree;
  }
