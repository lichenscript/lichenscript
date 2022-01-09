
module Loc = Lichenscript_lex.Loc

type t = {
  pident_name: string;
  pident_loc: Loc.t;
}
[@@deriving show]
