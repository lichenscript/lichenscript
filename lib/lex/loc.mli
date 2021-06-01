
type position = {
  line: int;
  column: int;
}

type t = {
  source : File_key.t option;
  start: position;
  _end:  position;
}

val mk_pos: int -> int -> position

val source : t -> File_key.t option

val pp : Format.formatter -> t -> unit
