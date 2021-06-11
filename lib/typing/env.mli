
type t

val create: unit -> t

val peek_scope: t -> Scope.t

val push_scope: t -> Scope.t -> unit

val pop_scope: t -> Scope.t option

val find_or_create_var_symbol: t -> string -> Core_type.VarSym.t

val find_or_create_type_symbol: t -> string -> Core_type.TypeSym.t
