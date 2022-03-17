
type position = {
  line: int;
  column: int;
  offset: int;
}

let pp_position formatter pos =
  Format.fprintf formatter "(%d:%d)" pos.line pos.column

type t = {
  source: File_key.t option;
  start: position;
  _end:  position;
}
[@@deriving show]

let none = {
  source = None;
  start = { line = 0; column = 0; offset = 0 };
  _end = { line = 0; column = 0; offset = 0 };
}

let btwn loc1 loc2 = { source = loc1.source; start = loc1.start; _end = loc2._end }

let mk_pos line column offset =
  { line; column; offset }

let source loc = loc.source
