open Lichenscript_lex
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
  export_var: Scope.variable;
}

(* prev export * new export *)
exception ReexportSymbol of (Loc.t * export * export)

type t

val create: full_path:string -> module_scope:Scope.scope -> unit -> t

val module_scope: t -> Scope.scope

val add_file: t -> file -> unit

val files: t -> file list

val set_files: t -> file list -> unit

val finalize_module_exports: t -> unit

val find_export: t -> name: string -> export option
