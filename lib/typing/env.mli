
type t

val create: unit -> t

val find_or_create_symbol: t -> string -> Symbol.t
