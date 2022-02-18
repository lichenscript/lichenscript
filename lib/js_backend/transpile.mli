open Lichenscript_typing

type t

val transpile_program: ctx:Type_context.t -> preclude:string -> Lichenscript_typing.Typedtree.Declaration.t list -> t

val contents: t -> string
