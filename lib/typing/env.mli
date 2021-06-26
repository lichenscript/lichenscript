
type t

val create: unit -> t

val set_current_scope: t -> Scope.t -> unit

val peek_scope: t -> Scope.t

val add_error: t -> Type_error.t -> unit

val with_new_scope: t -> Scope.t -> (t -> 'a) -> 'a
