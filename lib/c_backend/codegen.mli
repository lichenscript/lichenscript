open Lichenscript_typing
open Lichenscript_ir

type t

val codegen_program:
  ?indent: string ->
  prog:Program.t ->
  includes:string list ->
  init_calls:string list ->
  ptr_size:Ir.ptr_size ->
  Typedtree.Declaration.t list -> t

val contents: t -> string
