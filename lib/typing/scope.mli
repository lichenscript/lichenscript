
type t

val create: int -> t

val find_or_create_symbol: t -> string -> Symbol.t
