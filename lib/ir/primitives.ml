open Lichenscript_parsing

module Value = struct

  let mk_i32 = "MK_I32"

  let mk_f32 = "MK_F32"

  let new_string = "LCNewStringFromCString"

  let new_string_len = "LCNewStringFromCStringLen"

  let release = "LCRelease"

  let init_class_meta = "LC_init_class_meta"
  
end

module Constant = struct

  let _true = "LCTrue"

  let _false = "LCFalse"
  
end

module Bin = struct

  let prim (op: Asttypes.BinaryOp.t) =
    match op with
    | Equal -> "LC_I32_EQ"
    | NotEqual -> "LC_I32_NOT_EQ"
    | LessThan -> "LC_I32_LT"
    | LessThanEqual -> "LC_I32_LTEQ"
    | GreaterThan -> "LC_I32_GT"
    | GreaterThanEqual -> "LC_I32_GTEQ"
    | LShift -> "LC_I32_LEFT_SHIFT"
    | RShift -> "LC_I32_RIGHT_SHIFT"
    | Plus -> "LC_I32_PLUS"
    | Minus -> "LC_I32_MINUS"
    | Mult -> "LC_I32_MULT"
    | Div -> "LC_I32_DIV"
    | Mod -> "LC_I32_MOD"
    | BitOr -> "LC_I32_BIT_OR"
    | Xor -> "LC_I32_XOR"
    | BitAnd -> "LC_I32_BIT_AND"
    | And -> "LC_AND"
    | Or -> "LC_OR"

  let to_arithmetic_op (op: Asttypes.BinaryOp.t) =
    match op with
    | Plus -> "LC_ARTH_PLUS"
    | Minus -> "LC_ARTH_MINUS"
    | Mult -> "LC_ARTH_MULT"
    | Div -> "LC_ARTH_DIV"
    | Mod -> "LC_ARTH_MOD"
    | LShift -> "LC_ARTH_LSHIFT"
    | RShift -> "LC_ARTH_RSHIFT"
    | BitOr -> "LC_ARTH_BIT_OR"
    | Xor -> "LC_ARTH_BIT_XOR"
    | BitAnd -> "LC_I32_BIT_AND"
    | _ -> failwith "unsupport binary op for f32"

  let prim_f32 (op: Asttypes.BinaryOp.t) =
    match op with
    | Equal -> "LC_F32_EQ"
    | NotEqual -> "LC_F32_NOT_EQ"
    | LessThan -> "LC_F32_LT"
    | LessThanEqual -> "LC_F32_LTEQ"
    | GreaterThan -> "LC_F32_GT"
    | GreaterThanEqual -> "LC_F32_GTEQ"
    | Plus -> "LC_F32_PLUS"
    | Minus -> "LC_F32_MINUS"
    | Mult -> "LC_F32_MULT"
    | Div -> "LC_F32_DIV"
    | Mod -> "LC_I32_MOD"
    | _ -> failwith "unsupport binary op for f32"

  let to_cmp (op: Asttypes.BinaryOp.t) =
    match op with
    | Equal -> "LC_CMP_EQ"
    | NotEqual -> "LC_CMP_NEQ"
    | LessThan -> "LC_CMP_LT"
    | LessThanEqual -> "LC_CMP_LTEQ"
    | GreaterThan -> "LC_CMP_GT"
    | GreaterThanEqual -> "LC_CMP_GTEQ"
    | _ -> failwith "unreachable"

end

module Assign = struct
  
  let to_arithmetic_op (op: Asttypes.AssignOp.t) =
    match op with
    | PlusAssign -> "LC_ARTH_PLUS"
    | MinusAssign -> "LC_ARTH_MINUS"
    | MultAssign -> "LC_ARTH_MULT"
    | DivAssign -> "LC_ARTH_DIV"
    | ModAssign -> "LC_ARTH_MOD"
    | LShiftAssign -> "LC_ARTH_LSHIFT"
    | RShiftAssign -> "LC_ARTH_RSHIFT"
    | BitOrAssign -> "LC_ARTH_BIT_OR"
    | BitXorAssign -> "LC_ARTH_BIT_XOR"
    | BitAndAssign -> "LC_ARTH_BIT_AND"
  
end
