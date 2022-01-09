open Core
open Waterlang_parsing
open Waterlang_typing

type file = {
	path: string;
	ast: Ast.program option;
	typed_env: Env.t;
  typed_tree: Typedtree.program option;
	extern_modules: string list;  (* full id *)
}

type t = {
  mod_full_path: string;
  mutable files: file list;
}

let create ~full_path () =
  {
    mod_full_path = full_path;
    files = [];
  }

let add_file env file =
  env.files <- file::env.files

let files env = List.rev env.files

let set_files env files =
  env.files <- files
