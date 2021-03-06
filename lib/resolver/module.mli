open Core_kernel
open Lichenscript_lex
open Lichenscript_parsing
open Lichenscript_typing


type file = {
  path: string;
  ast: Ast.program option;  (* release later *)
  typed_env: Env.t;
  typed_tree: Typedtree.program option;
  import_star_external_modules: string list;  (* full id *)

  (* relative_map -> absolute map *)
  imports_map: (string, string) Hashtbl.t;
}

type export = {
  export_name: string;
  export_var: Scope.variable;
}

(* prev export * new export *)
exception ReexportSymbol of (Loc.t * export * export)

type t

val create: full_path:string -> is_std:bool -> module_scope:Scope.scope -> unit -> t

val path: t-> string

val is_std: t -> bool

val module_scope: t -> Scope.scope

val add_file: t -> file -> unit

val files: t -> file list

val set_files: t -> file list -> unit

val finalize_module_exports: t -> unit

val find_export: t -> name: string -> export option

val exports: t -> (string * export) list
