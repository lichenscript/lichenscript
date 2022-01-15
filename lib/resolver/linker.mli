open Lichenscript_typing

type t

val create: ctx:Type_context.t -> unit -> t

val link_from_entry: t -> int -> Typedtree.Declaration.t list

val set_module: t -> string -> Module.t -> unit

val get_module: t -> string -> Module.t option

val has_module: t -> string -> bool

val iter_modules: t -> f:(Module.t -> unit) -> unit
