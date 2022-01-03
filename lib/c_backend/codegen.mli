
type t

val create: ?indent: string -> unit -> t

val codegen_program: t -> Waterlang_typing.Typedtree.program -> unit

val contents: t -> string
