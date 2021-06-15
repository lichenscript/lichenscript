
type t

val id: t -> int

val create: int -> t

val find_or_create_var_symbol: t -> string -> Core_type.VarSym.t

val find_or_create_type_symbol: t -> string -> Core_type.TypeSym.t

val find_type_symbol: t -> string -> Core_type.TypeSym.t option

val set_type_symbol: t -> string -> Core_type.TypeSym.t -> unit
