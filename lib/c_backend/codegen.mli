open Lichenscript_typing

type t

val codegen_program: ?indent: string -> ctx:Type_context.t -> Lichenscript_typing.Typedtree.Declaration.t list -> t

val contents: t -> string
