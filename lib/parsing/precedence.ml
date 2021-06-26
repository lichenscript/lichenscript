open Waterlang_lex

let binary_precedence = function
  | Token.T_LPAREN
  | Token.T_SEMICOLON
  | Token.T_COMMA
  | Token.T_ASSIGN
  | Token.T_RBRACKET
    -> 0

  | Token.T_OR -> 1
  | Token.T_AND -> 2
  | Token.T_BIT_OR -> 3
  | Token.T_BIT_XOR -> 4
  | Token.T_BIT_AND -> 5

  | Token.T_EQUAL
  | Token.T_NOT_EQUAL
  | Token.T_STRICT_EQUAL
  | Token.T_STRICT_NOT_EQUAL
    -> 6

  | Token.T_LESS_THAN
  | Token.T_GREATER_THAN
  | Token.T_LESS_THAN_EQUAL
  | Token.T_GREATER_THAN_EQUAL
    -> 7

  | Token.T_RSHIFT
  | Token.T_LSHIFT
  | Token.T_RSHIFT3
    -> 8

  | Token.T_PLUS
  | Token.T_MINUS
    -> 9

  | Token.T_MULT
  | Token.T_DIV
  | Token.T_MOD
    -> 11

  | Token.T_INSTANCEOF
  | Token.T_IN
    -> 7

  | _ -> 0