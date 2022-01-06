open Waterlang_lex

type tuple = (string * Loc.t)
[@@deriving show]

type t = {
  names: tuple list;
}
[@@deriving show]
