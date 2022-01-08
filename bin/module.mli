open Waterlang_parsing
open Waterlang_typing

type file = {
	path: string;
	ast: Ast.program option;  (* release later *)
	typed_env: Env.t;
  typed_tree: Typedtree.program option;
}

type t

val get_id_str: string array -> string

val create: id:string array -> id_str: string -> unit -> t

val add_file: t -> file -> unit

val files: t -> file list

val set_files: t -> file list -> unit
