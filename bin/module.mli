open Waterlang_typing

type t

val get_id_str: string array -> string

val create: path:string -> id:string array -> id_str: string -> Typedtree.program -> t
