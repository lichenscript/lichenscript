
type t = {
  lex_token: Token.t;
  lex_loc: Loc.t;
  lex_errors: (Loc.t * Lex_error.t) list;
  lex_comments: Loc.t Comment.t list;
}

val token: t -> Token.t

val loc: t -> Loc.t

val comments: t -> Loc.t Comment.t list

val error: t -> (Loc.t * Lex_error.t) list

val debug_string_of_lex_result: t -> string
