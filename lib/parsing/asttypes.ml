
module Loc = Lichenscript_lex.Loc

type constant =
  Const_int of int
| Const_char of char
| Const_string of string * Loc.t * string option
| Const_float of string
| Const_int32 of int32
| Const_int64 of int64
| Const_nativeint of nativeint
[@@deriving show]

type 'a loc = {
  txt : 'a;
  loc : Loc.t;
}
[@@deriving show]

module UnaryOp = struct

  type t =
    | Minus
    | Plus
    | Not
    | BitNot
    [@@deriving show]

  let from_token =
    let open Lichenscript_lex in
    function
    | Token.T_PLUS -> Plus
    | Token.T_MINUS -> Minus
    | Token.T_BIT_NOT -> BitNot
    | Token.T_NOT -> Not
    | _ -> failwith "unreachable"
  
end

module BinaryOp = struct

  type t =
    | Equal
    | NotEqual
    | StrictEqual
    | StrictNotEqual
    | LessThan
    | LessThanEqual
    | GreaterThan
    | GreaterThanEqual
    | LShift
    | RShift
    | RShift3
    | Plus
    | Minus
    | Mult
    | Exp
    | Div
    | Mod
    | BitOr
    | Xor
    | BitAnd
    [@@deriving show]

  let from_token =
    let open Lichenscript_lex in
    function
    | Token.T_EQUAL -> Equal
    | Token.T_NOT_EQUAL -> NotEqual
    | Token.T_STRICT_EQUAL -> StrictEqual
    | Token.T_STRICT_NOT_EQUAL -> StrictNotEqual
    | Token.T_LESS_THAN -> LessThan
    | Token.T_LESS_THAN_EQUAL -> LessThanEqual
    | Token.T_GREATER_THAN -> GreaterThan
    | Token.T_GREATER_THAN_EQUAL -> GreaterThanEqual
    | Token.T_LSHIFT -> LShift
    | Token.T_RSHIFT -> RShift
    | Token.T_RSHIFT3 -> RShift3
    | Token.T_PLUS -> Plus
    | Token.T_MINUS -> Minus
    | Token.T_MULT -> Mult
    | Token.T_EXP -> Exp
    | Token.T_DIV -> Div
    | Token.T_MOD -> Mod
    | Token.T_BIT_OR -> BitOr
    | Token.T_BIT_XOR -> Xor
    | Token.T_BIT_AND -> BitAnd
    | _ -> failwith "unreachable"
  
end

module UpdateOp = struct

  type t =
    | Increase
    | Decrease
    [@@deriving show]
  
end

type visibility =
  | Pvisibility_public
  | Pvisibility_protected
  | Pvisibility_private
  [@@deriving show]
