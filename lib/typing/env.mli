
type t

val create: unit -> t

val peek_scope: t -> Scope.t

val push_scope: t -> Scope.t -> unit

val pop_scope: t -> Scope.t option

val bind_type: t -> sym_id:int -> Core_type.core_type -> unit

val find_or_create_var_symbol: t -> string -> Symbol.var_sym

val find_or_create_type_symbol: t -> string -> Symbol.type_sym
