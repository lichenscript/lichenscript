
type t =
  | T_INTEGER of {
      kind: number_type;
      raw: string;
      content: string;
      is_long: bool;
    }
  | T_FLOAT of {
    raw: string;
    content: string;
    is_f32: bool;
  }

  | T_CHAR of (Loc.t * int * string)
  | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
  | T_TEMPLATE_PART of (Loc.t * template_part * bool) (* loc, value, is_tail *)
  | T_IDENTIFIER of {
      loc: Loc.t;
      value: string;
      raw: string;
    }
  | T_REGEXP of (Loc.t * string * string) (* /pattern/flags *)
  (* Syntax *)
  | T_LCURLY
  | T_RCURLY
  | T_LPAREN
  | T_RPAREN
  | T_LBRACKET
  | T_RBRACKET
  | T_SEMICOLON
  | T_COMMA
  | T_PERIOD
  | T_ARROW
  | T_ELLIPSIS
  | T_AT
  | T_POUND
  (* Keywords *)
  | T_AS
  | T_FUNCTION
  | T_IF
  | T_IN
  | T_RETURN
  | T_THIS
  | T_THROW
  | T_WHILE
  | T_CONST
  | T_LET
  | T_FALSE
  | T_TRUE
  | T_BREAK
  | T_CASE
  | T_CONTINUE
  | T_DEFAULT
  | T_DO
  | T_FINAL
  | T_FOR
  | T_CLASS
  | T_EXTENDS
  | T_OBJECT
  | T_STATIC
  | T_ELSE
  | T_VIRTUAL
  | T_OVERRIDE
  | T_VOID
  | T_ENUM
  | T_MATCH
  | T_EXPORT
  | T_IMPORT
  | T_FROM
  | T_SUPER
  | T_IMPLEMENTS
  | T_INTERFACE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_INTERNAL
  | T_MODULE
  | T_YIELD
  | T_DEBUGGER
  | T_DECLARE
  | T_TYPE
  | T_OF
  | T_ASYNC
  | T_AWAIT
  (* Operators *)
  | T_RSHIFT_ASSIGN
  | T_LSHIFT_ASSIGN
  | T_BIT_XOR_ASSIGN
  | T_BIT_OR_ASSIGN
  | T_BIT_AND_ASSIGN
  | T_MOD_ASSIGN
  | T_DIV_ASSIGN
  | T_MULT_ASSIGN
  | T_MINUS_ASSIGN
  | T_PLUS_ASSIGN
  | T_ASSIGN
  | T_PLING
  | T_COLON
  | T_OR
  | T_AND
  | T_BIT_OR
  | T_BIT_XOR
  | T_BIT_AND
  | T_EQUAL
  | T_NOT_EQUAL
  | T_STRICT_EQUAL
  | T_STRICT_NOT_EQUAL
  | T_LESS_THAN_EQUAL
  | T_GREATER_THAN_EQUAL
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LSHIFT
  | T_RSHIFT
  | T_PLUS
  | T_MINUS
  | T_DIV
  | T_MULT
  | T_MOD
  | T_NOT
  | T_BIT_NOT
  | T_INCR
  | T_DECR
  (* Extra tokens *)
  | T_ERROR of string
  | T_EOF
  (* JSX *)
  | T_JSX_IDENTIFIER of { raw: string }
  | T_JSX_TEXT of (Loc.t * string * string) (* loc, value, raw *)

and number_type =
  | BINARY
  | OCTAL
  | NORMAL

and template_part = {
  cooked: string;
  (* string after processing special chars *)
  raw: string;
  (* string as specified in source *)
  literal: string; (* same as raw, plus characters like ` and ${ *)
}

(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)
let token_to_string = function
  | T_INTEGER _ -> "T_INTEGER"
  | T_FLOAT _ -> "T_FLOAT"
  | T_CHAR _ -> "T_CHAR"
  | T_STRING _ -> "T_STRING"
  | T_TEMPLATE_PART _ -> "T_TEMPLATE_PART"
  | T_IDENTIFIER _ -> "T_IDENTIFIER"
  | T_REGEXP _ -> "T_REGEXP"
  | T_AS -> "T_AS"
  | T_FUNCTION -> "T_FUNCTION"
  | T_IF -> "T_IF"
  | T_IN -> "T_IN"
  | T_RETURN -> "T_RETURN"
  | T_THIS -> "T_THIS"
  | T_THROW -> "T_THROW"
  | T_WHILE -> "T_WHILE"
  | T_CONST -> "T_CONST"
  | T_LET -> "T_LET"
  | T_FALSE -> "T_FALSE"
  | T_TRUE -> "T_TRUE"
  | T_BREAK -> "T_BREAK"
  | T_CASE -> "T_CASE"
  | T_CONTINUE -> "T_CONTINUE"
  | T_DEFAULT -> "T_DEFAULT"
  | T_DO -> "T_DO"
  | T_FINAL -> "T_FINAL"
  | T_FOR -> "T_FOR"
  | T_CLASS -> "T_CLASS"
  | T_EXTENDS -> "T_EXTENDS"
  | T_OBJECT -> "T_OBJECT"
  | T_STATIC -> "T_STATIC"
  | T_ELSE -> "T_ELSE"
  | T_VIRTUAL -> "T_VIRTUAL"
  | T_OVERRIDE -> "T_OVERRIDE"
  | T_VOID -> "T_VOID"
  | T_ENUM -> "T_ENUM"
  | T_MATCH -> "T_MATCH"
  | T_EXPORT -> "T_EXPORT"
  | T_IMPORT -> "T_IMPORT"
  | T_FROM -> "T_FROM"
  | T_SUPER -> "T_SUPER"
  | T_IMPLEMENTS -> "T_IMPLEMENTS"
  | T_INTERFACE -> "T_INTERFACE"
  | T_PRIVATE -> "T_PRIVATE"
  | T_PROTECTED -> "T_PROTECTED"
  | T_PUBLIC -> "T_PUBLIC"
  | T_INTERNAL -> "T_PUBLIC"
  | T_MODULE -> "T_MODULE"
  | T_YIELD -> "T_YIELD"
  | T_DEBUGGER -> "T_DEBUGGER"
  | T_DECLARE -> "T_DECLARE"
  | T_TYPE -> "T_TYPE"
  | T_OF -> "T_OF"
  | T_ASYNC -> "T_ASYNC"
  | T_AWAIT -> "T_AWAIT"
  | T_LCURLY -> "T_LCURLY"
  | T_RCURLY -> "T_RCURLY"
  | T_LPAREN -> "T_LPAREN"
  | T_RPAREN -> "T_RPAREN"
  | T_LBRACKET -> "T_LBRACKET"
  | T_RBRACKET -> "T_RBRACKET"
  | T_SEMICOLON -> "T_SEMICOLON"
  | T_COMMA -> "T_COMMA"
  | T_PERIOD -> "T_PERIOD"
  | T_ARROW -> "T_ARROW"
  | T_ELLIPSIS -> "T_ELLIPSIS"
  | T_AT -> "T_AT"
  | T_POUND -> "T_POUND"
  | T_RSHIFT_ASSIGN -> "T_RSHIFT_ASSIGN"
  | T_LSHIFT_ASSIGN -> "T_LSHIFT_ASSIGN"
  | T_BIT_XOR_ASSIGN -> "T_BIT_XOR_ASSIGN"
  | T_BIT_OR_ASSIGN -> "T_BIT_OR_ASSIGN"
  | T_BIT_AND_ASSIGN -> "T_BIT_AND_ASSIGN"
  | T_MOD_ASSIGN -> "T_MOD_ASSIGN"
  | T_DIV_ASSIGN -> "T_DIV_ASSIGN"
  | T_MULT_ASSIGN -> "T_MULT_ASSIGN"
  | T_MINUS_ASSIGN -> "T_MINUS_ASSIGN"
  | T_PLUS_ASSIGN -> "T_PLUS_ASSIGN"
  | T_ASSIGN -> "T_ASSIGN"
  | T_PLING -> "T_PLING"
  | T_COLON -> "T_COLON"
  | T_OR -> "T_OR"
  | T_AND -> "T_AND"
  | T_BIT_OR -> "T_BIT_OR"
  | T_BIT_XOR -> "T_BIT_XOR"
  | T_BIT_AND -> "T_BIT_AND"
  | T_EQUAL -> "T_EQUAL"
  | T_NOT_EQUAL -> "T_NOT_EQUAL"
  | T_STRICT_EQUAL -> "T_STRICT_EQUAL"
  | T_STRICT_NOT_EQUAL -> "T_STRICT_NOT_EQUAL"
  | T_LESS_THAN_EQUAL -> "T_LESS_THAN_EQUAL"
  | T_GREATER_THAN_EQUAL -> "T_GREATER_THAN_EQUAL"
  | T_LESS_THAN -> "T_LESS_THAN"
  | T_GREATER_THAN -> "T_GREATER_THAN"
  | T_LSHIFT -> "T_LSHIFT"
  | T_RSHIFT -> "T_RSHIFT"
  | T_PLUS -> "T_PLUS"
  | T_MINUS -> "T_MINUS"
  | T_DIV -> "T_DIV"
  | T_MULT -> "T_MULT"
  | T_MOD -> "T_MOD"
  | T_NOT -> "T_NOT"
  | T_BIT_NOT -> "T_BIT_NOT"
  | T_INCR -> "T_INCR"
  | T_DECR -> "T_DECR"
  (* Extra tokens *)
  | T_ERROR _ -> "T_ERROR"
  | T_EOF -> "T_EOF"
  | T_JSX_IDENTIFIER _ -> "T_JSX_IDENTIFIER"
  | T_JSX_TEXT _ -> "T_JSX_TEXT"

let value_of_token = function
  | T_INTEGER { raw; _ } -> raw
  | T_FLOAT { raw; _ } -> raw
  | T_CHAR (_, _, raw) -> raw
  | T_STRING (_, _, raw, _) -> raw
  | T_TEMPLATE_PART (_, { literal; _ }, _) -> literal
  | T_IDENTIFIER { raw; _ } -> raw
  | T_REGEXP (_, pattern, flags) -> "/" ^ pattern ^ "/" ^ flags
  | T_LCURLY -> "{"
  | T_RCURLY -> "}"
  | T_LPAREN -> "("
  | T_RPAREN -> ")"
  | T_LBRACKET -> "["
  | T_RBRACKET -> "]"
  | T_SEMICOLON -> ";"
  | T_COMMA -> ","
  | T_PERIOD -> "."
  | T_ARROW -> "=>"
  | T_ELLIPSIS -> "..."
  | T_AT -> "@"
  | T_POUND -> "#"
  | T_AS -> "as"
  | T_FUNCTION -> "function"
  | T_IF -> "if"
  | T_IN -> "in"
  | T_RETURN -> "return"
  | T_THIS -> "this"
  | T_THROW -> "throw"
  | T_WHILE -> "while"
  | T_CONST -> "const"
  | T_LET -> "let"
  | T_FALSE -> "false"
  | T_TRUE -> "true"
  | T_BREAK -> "break"
  | T_CASE -> "case"
  | T_CONTINUE -> "continue"
  | T_DEFAULT -> "default"
  | T_DO -> "do"
  | T_FINAL -> "final"
  | T_FOR -> "for"
  | T_CLASS -> "class"
  | T_EXTENDS -> "extends"
  | T_OBJECT -> "object"
  | T_STATIC -> "static"
  | T_ELSE -> "else"
  | T_VIRTUAL -> "virtual"
  | T_OVERRIDE -> "override"
  | T_VOID -> "void"
  | T_ENUM -> "enum"
  | T_MATCH -> "match"
  | T_EXPORT -> "export"
  | T_IMPORT -> "import"
  | T_FROM -> "from"
  | T_SUPER -> "super"
  | T_IMPLEMENTS -> "implements"
  | T_INTERFACE -> "interface"
  | T_PRIVATE -> "private"
  | T_PROTECTED -> "protected"
  | T_PUBLIC -> "public"
  | T_INTERNAL -> "internal"
  | T_MODULE -> "module"
  | T_YIELD -> "yield"
  | T_DEBUGGER -> "debugger"
  | T_DECLARE -> "declare"
  | T_TYPE -> "type"
  | T_OF -> "of"
  | T_ASYNC -> "async"
  | T_AWAIT -> "await"
  | T_RSHIFT_ASSIGN -> ">>="
  | T_LSHIFT_ASSIGN -> "<<="
  | T_BIT_XOR_ASSIGN -> "^="
  | T_BIT_OR_ASSIGN -> "|="
  | T_BIT_AND_ASSIGN -> "&="
  | T_MOD_ASSIGN -> "%="
  | T_DIV_ASSIGN -> "/="
  | T_MULT_ASSIGN -> "*="
  | T_MINUS_ASSIGN -> "-="
  | T_PLUS_ASSIGN -> "+="
  | T_ASSIGN -> "="
  | T_PLING -> "?"
  | T_COLON -> ":"
  | T_OR -> "||"
  | T_AND -> "&&"
  | T_BIT_OR -> "|"
  | T_BIT_XOR -> "^"
  | T_BIT_AND -> "&"
  | T_EQUAL -> "=="
  | T_NOT_EQUAL -> "!="
  | T_STRICT_EQUAL -> "==="
  | T_STRICT_NOT_EQUAL -> "!=="
  | T_LESS_THAN_EQUAL -> "<="
  | T_GREATER_THAN_EQUAL -> ">="
  | T_LESS_THAN -> "<"
  | T_GREATER_THAN -> ">"
  | T_LSHIFT -> "<<"
  | T_RSHIFT -> ">>"
  | T_PLUS -> "+"
  | T_MINUS -> "-"
  | T_DIV -> "/"
  | T_MULT -> "*"
  | T_MOD -> "%"
  | T_NOT -> "!"
  | T_BIT_NOT -> "~"
  | T_INCR -> "++"
  | T_DECR -> "--"
  (* Extra tokens *)
  | T_ERROR raw -> raw
  | T_EOF -> ""
  | T_JSX_IDENTIFIER { raw } -> raw
  | T_JSX_TEXT (_, _, raw) -> raw

let quote_token_value value = Printf.sprintf "token `%s`" value

let explanation_of_token ?(use_article = false) token =
  let (value, article) =
    match token with
    | T_INTEGER _ ->
      ("integer", "a")
    | T_FLOAT _ ->
      ("float", "a")
    | T_JSX_TEXT _
    | T_STRING _ ->
      ("string", "a")
    | T_TEMPLATE_PART _ -> ("template literal part", "a")
    | T_JSX_IDENTIFIER _
    | T_IDENTIFIER _ ->
      ("identifier", "an")
    | T_REGEXP _ -> ("regexp", "a")
    | T_EOF -> ("end of input", "the")
    | _ -> (quote_token_value (value_of_token token), "the")
  in
  if use_article then
    article ^ " " ^ value
  else
    value

let is_keyword = function
  | T_FUNCTION
  | T_IF
  | T_IN
  | T_RETURN
  | T_THIS
  | T_THROW
  | T_WHILE
  | T_CONST
  | T_LET
  | T_FALSE
  | T_TRUE
  | T_BREAK
  | T_CASE
  | T_CONTINUE
  | T_DEFAULT
  | T_DO
  | T_FINAL
  | T_FOR
  | T_CLASS
  | T_EXTENDS
  | T_STATIC
  | T_ELSE
  | T_VOID
  | T_ENUM
  | T_EXPORT
  | T_IMPORT
  | T_SUPER
  | T_IMPLEMENTS
  | T_INTERFACE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_YIELD
  | T_DEBUGGER
  | T_DECLARE
  | T_TYPE
  | T_OF
  | T_ASYNC
  | T_AWAIT -> true
  | _ -> false
