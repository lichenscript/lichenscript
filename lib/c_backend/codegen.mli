open Lichenscript_typing

type t

val codegen_program: ?indent: string -> prog:Program.t -> includes:string list -> Lichenscript_typing.Typedtree.Declaration.t list -> t

val contents: t -> string
