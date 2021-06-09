
type t

val create: unit -> t

val peek_scope: t -> Scope.t

val push_scope: t -> Scope.t -> unit

val pop_scope: t -> Scope.t option

val set_symbol_type: t -> int -> Core_type.core_type -> unit

val find_or_create_var_symbol: t -> string -> Symbol.t
