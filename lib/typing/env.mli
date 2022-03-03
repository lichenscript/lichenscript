open Scope

type t

type external_resolver = string -> name:string -> int option

val create: external_resolver:external_resolver -> file_scope:scope -> Type_context.t -> t

val ctx : t -> Type_context.t

val in_lambda: t -> bool

val set_in_lambda: t -> bool -> unit

val capture_variable: t -> name:string -> unit

val set_current_scope: t -> scope -> unit

val peek_scope: t -> scope

val file_scope: t -> scope

val add_error: t -> Type_error.t -> unit

val errors: t -> Type_error.t list

val with_new_scope: t -> scope -> (t -> 'a) -> 'a

val external_resolver: t -> external_resolver

val ty_u32: t -> int

val ty_i32: t -> int

val ty_f32: t -> int

val ty_char: t -> int

val ty_boolean: t -> int
