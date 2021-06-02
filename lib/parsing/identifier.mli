
module Loc = Waterlang_lex.Loc

type t = {
  pident_name: string;
  pstmt_loc: Loc.t;
}
