open Lichenscript_typing

type t

val codegen_program: ?indent: string -> prog:Program.t -> includes:string list -> init_calls:string list -> Typedtree.Declaration.t list -> t

val contents: t -> string
