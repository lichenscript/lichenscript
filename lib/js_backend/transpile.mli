open Lichenscript_typing

type t

val transpile_program: prog:Program.t -> preclude:string -> init_calls:string list -> Typedtree.Declaration.t list -> t

val contents: t -> string
