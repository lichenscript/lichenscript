open Lichenscript_typing

type t

val create: prog:Program.t -> unit -> t

val link_from_entry: t -> verbose:bool -> int -> Typedtree.Declaration.t list

val set_module: t -> string -> Module.t -> unit

val get_module: t -> string -> Module.t option

val has_module: t -> string -> bool

val iter_modules: t -> f:(Module.t -> unit) -> unit

val add_external_resource: t -> string -> unit

val external_resources: t -> string list
