
type position = {
  line: int;
  column: int;
}
[@@deriving show]

type t = {
  source: File_key.t option;
  start: position;
  _end:  position;
}
[@@deriving show]

let mk_pos line column =
  { line; column }

let source loc = loc.source
