open Waterlang_typing

type file = {
	path: string;
  typed_tree: Typedtree.program;
}

type t

val get_id_str: string array -> string

val create: id:string array -> id_str: string -> unit -> t

val add_file: t -> file -> unit

val files: t -> file list

val set_files: t -> file list -> unit
