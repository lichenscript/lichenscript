
module Parse_error = struct
  type t = int
end

type t = {
  lex_source: File_key.t option;
  lex_lb: Sedlexing.lexbuf;
  lex_bol: bol;
  lex_in_comment_syntax: bool;
  lex_enable_comment_syntax: bool;
  lex_state: lex_state;
  lex_last_loc: Loc.t;
}

(* bol = Beginning Of Line *)
and bol = {
  line: int;
  offset: int;
}

and lex_state = { lex_errors_acc: (Loc.t * Parse_error.t) list }

let empty_lex_state = { lex_errors_acc = [] }

(* The lex_last_loc should initially be set to the beginning of the first line, so that
   comments on the first line are reported as not being on a new line. *)
let initial_last_loc =
  { Loc.source = None; start = { Loc.line = 1; column = 0 }; _end = { Loc.line = 1; column = 0 } }

let new_lex_env lex_source lex_lb ~enable_types_in_comments =
  {
    lex_source;
    lex_lb;
    lex_bol = { line = 1; offset = 0 };
    lex_in_comment_syntax = false;
    lex_enable_comment_syntax = enable_types_in_comments;
    lex_state = empty_lex_state;
    lex_last_loc = initial_last_loc;
  }

let line env = env.lex_bol.line

let source env = env.lex_source

let bol_offset env = env.lex_bol.offset
