open Lichenscript_typing

type t

val create: ?indent: string -> ctx:Type_context.t -> unit -> t

val codegen_program: t -> Lichenscript_typing.Typedtree.program -> unit

val contents: t -> string
