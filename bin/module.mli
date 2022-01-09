open Waterlang_parsing
open Waterlang_typing

type file = {
	path: string;
	ast: Ast.program option;  (* release later *)
	typed_env: Env.t;
  typed_tree: Typedtree.program option;
	extern_modules: string list;  (* full id *)
}

type t

val create: full_path: string -> unit -> t

val add_file: t -> file -> unit

val files: t -> file list

val set_files: t -> file list -> unit
