
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

let none = { source = None; start = { line = 0; column = 0 }; _end = { line = 0; column = 0 } }

let mk_pos line column =
  { line; column }

let source loc = loc.source
