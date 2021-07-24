open Waterlang_lex

type t = {
  perr_loc: Loc.t;
  perr_spec: spec;
}

and spec =
  | LexError of Waterlang_lex.Lex_error.t
  | IsNotLeftValue
  | MalformedUnicode

exception Error of t list

let error t = raise (Error [t])

module PP = struct

  let error err =
    let { perr_spec; _ } = err in
    match perr_spec with
    | LexError lex_err -> Lex_error.PP.error lex_err
    | IsNotLeftValue -> "Element is not a left value"
    | MalformedUnicode -> "Malformed unicode"

end
