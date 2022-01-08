
type t

val create: ?open_domains:(string array list) -> type_provider:Type_provider.provider -> Type_context.t -> t

val ctx : t -> Type_context.t

val set_current_scope: t -> Scope.t -> unit

val peek_scope: t -> Scope.t

val module_scope: t -> Scope.t

val add_error: t -> Type_error.t -> unit

val errors: t -> Type_error.t list

val with_new_scope: t -> Scope.t -> (t -> 'a) -> 'a

val add_return_type: t -> int -> unit

val take_return_types: t -> int list

val ty_u32: t -> int

val ty_i32: t -> int

val ty_f32: t -> int

val ty_char: t -> int

val ty_string: t -> int

val ty_boolean: t -> int
