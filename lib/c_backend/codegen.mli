open Lichenscript_typing

type t

val codegen_program: ?indent: string -> ctx:Type_context.t -> Lichenscript_typing.Typedtree.program -> t

val contents: t -> string
