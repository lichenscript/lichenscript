open Lichenscript_typing

type t

val transpile_program: prog:Program.t -> preclude:string -> Lichenscript_typing.Typedtree.Declaration.t list -> t

val contents: t -> string
