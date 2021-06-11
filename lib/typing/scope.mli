
type t

val id: t -> int

val create: int -> t

val find_or_create_var_symbol: t -> string -> Symbol.var_sym

val find_or_create_type_symbol: t -> string -> Symbol.type_sym

val set_type_symbol: t -> string -> Symbol.type_sym -> unit
