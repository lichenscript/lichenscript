
type t =
  | Unexpected of string
  | UnexpectedWithExpected of string * string
  | UnexpectedTokenWithSuggestion of string * string
  | UnexpectedEOS
  | UnterminatedRegExp
  | InvalidFloatBigInt
  | InvalidSciBigInt
  | IllegalUnicodeEscape

module PP = struct

  let error = function
    | Unexpected unexpected -> Printf.sprintf "Unexpected %s" unexpected
    | UnexpectedWithExpected (unexpected, expected) ->
      Printf.sprintf "Unexpected %s, expected %s" unexpected expected
    | UnexpectedTokenWithSuggestion (token, suggestion) ->
      Printf.sprintf "Unexpected token `%s`. Did you mean `%s`?" token suggestion
    | UnexpectedEOS -> "Unexpected end of input"
    | UnterminatedRegExp -> "Invalid regular expression: missing /"
    | InvalidFloatBigInt -> "A bigint literal must be an integer"
    | InvalidSciBigInt -> "A bigint literal cannot use exponential notation"
    | IllegalUnicodeEscape -> "Illegal Unicode escape"

end
