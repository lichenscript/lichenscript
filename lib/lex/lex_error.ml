
type t =
  | Unexpected of string
  | UnexpectedWithExpected of string * string
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedEOS
  | UnterminatedRegExp
  | InvalidFloatBigInt
  | InvalidSciBigInt
  | IllegalUnicodeEscape
