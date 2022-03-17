
type position = {
  line: int;
  column: int;
  offset: int;
}

type t = {
  source : File_key.t option;
  start: position;
  _end:  position;
}

val none : t

val btwn : t -> t -> t

val mk_pos: int -> int -> int -> position

val source : t -> File_key.t option

val pp : Format.formatter -> t -> unit
