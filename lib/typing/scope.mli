
type t

val id: t -> int

val create: int -> t

val find_or_create_var_symbol: t -> string -> Symbol.t

val set_type_symbol: t -> string -> Symbol.t -> unit
