open Waterlang_lex

type t = {
  perr_loc: Loc.t;
  perr_spec: spec;
}

and spec =
  | LexError of Waterlang_lex.Lex_error.t
  | IsNotLeftValue
  | MalformedUnicode
  | VisibilityNoOnTopLevel

exception Error of t list

let error t = raise (Error [t])

module PP = struct

  let error formatter err =
    let { perr_spec; _ } = err in
    match perr_spec with
    | LexError lex_err -> Format.fprintf formatter "%s" (Lex_error.PP.error lex_err)
    | IsNotLeftValue -> Format.fprintf formatter "Element is not a left value"
    | MalformedUnicode -> Format.fprintf formatter "Malformed unicode"
    | VisibilityNoOnTopLevel -> Format.fprintf formatter "Visibility modifiers are only allowed on top-leveled scope"

end
