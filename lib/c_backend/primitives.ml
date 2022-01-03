open Waterlang_parsing

module Value = struct

  let mk_i32 = "MK_I32"

  let mk_f32 = "MK_F32"

  let new_string = "WTNewStringFromCString"

  let new_string_len = "WTNewStringFromCStringLen"
  
end

module Constant = struct

  let _true = "WTTrue"

  let _false = "WTFalse"
  
end

module Bin = struct

  let prim (op: Asttypes.BinaryOp.t) =
    match op with
    | Equal -> "WT_I32_EQ"
    | NotEqual -> "WT_I32_NOT_EQ"
    | StrictEqual
    | StrictNotEqual -> failwith "not support"
    | LessThan -> "WT_I32_LT"
    | LessThanEqual -> "WT_I32_LTEQ"
    | GreaterThan -> "WT_I32_GT"
    | GreaterThanEqual -> "WT_I32_GTEQ"
    | LShift -> "WT_I32_LEFT_SHIFT"
    | RShift -> "WT_I32_RIGHT_SHIFT"
    | RShift3 -> failwith "not support"
    | Plus -> "WT_I32_PLUS"
    | Minus -> "WT_I32_MINUX"
    | Mult -> "WT_I32_MULT"
    | Exp -> "WT_I32_EXP"
    | Div -> "WT_I32_DIV"
    | Mod -> "WT_I32_MOD"
    | BitOr -> "WT_I32_BIT_OR"
    | Xor -> "WT_I32_XOR"
    | BitAnd -> "WT_I32_BIT_AND"
  
end
