open Waterlang_lex

type t =
  | LexError of Waterlang_lex.Lex_error.t
  | MalformedUnicode

exception Error of (Loc.t * t) list

let error loc e = raise (Error [(loc, e)])

module PP = struct

  let error = function
    | LexError lex_err -> Lex_error.PP.error lex_err
    | MalformedUnicode -> "Malformed unicode"

end
