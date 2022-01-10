open Lichenscript_parsing
open Lichenscript_typing


type file = {
  path: string;
  ast: Ast.program option;  (* release later *)
  typed_env: Env.t;
  typed_tree: Typedtree.program option;
  extern_modules: string list;  (* full id *)
}

type export = {
  export_name: string;
  export_ty_var: int;
}

(* prev export * new export *)
exception ReexportSymbol of (export * export)

type t

val create: full_path: string -> unit -> t

val add_file: t -> file -> unit

val files: t -> file list

val set_files: t -> file list -> unit

val finalize_module_exports: t -> unit

val find_export: t -> name: string -> export option
