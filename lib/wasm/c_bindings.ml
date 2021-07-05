
type m

type ty

type global

type literal

type op = Int32.t

type exp

type function_

type export

external make_module: unit -> m = "make_module"

external module_emit_text: m -> string = "module_emit_text"

external module_emit_binary_to_file: m -> string -> unit = "module_emit_binary"

external make_ty_none: unit -> ty = "make_ty_none"

external make_ty_int32: unit -> ty = "make_ty_int32"

external make_ty_int64: unit -> ty = "make_ty_int64"

external make_ty_f32: unit -> ty = "make_ty_f32"

external make_ty_f64: unit -> ty = "make_ty_f64"

external make_ty_any_ref: unit -> ty = "make_ty_any_ref"

external make_ty_unreachable: unit -> ty = "make_ty_unreachable"

external make_ty_multiples: ty array -> ty = "make_ty_multiples"

external make_op_add_i32: unit -> op = "make_op_add_i32"

external make_op_sub_i32: unit -> op = "make_op_sub_i32"

external make_op_mul_i32: unit -> op = "make_op_mul_i32"

external make_literal_i32: Int32.t -> literal = "make_literal_i32"

external make_literal_i64: Int64.t -> literal = "make_literal_i64"

external make_literal_f32: Float.t -> literal = "make_literal_f32"

external make_literal_f64: Float.t -> literal = "make_literal_f64"

external make_exp_const: m -> literal -> exp = "make_exp_const"

external make_exp_binary: m -> op -> exp -> exp -> exp = "make_exp_binary"

external make_exp_unrechable: m -> exp = "make_exp_unreachable"

external make_exp_return: m -> exp option -> exp = "make_exp_return"

external make_exp_local_get: m -> int -> ty -> exp = "make_exp_local_get"

external make_exp_call: m -> string -> exp array -> ty -> exp = "make_exp_call"

external add_function: m -> string -> ty -> ty -> ty array -> exp -> function_ =
  "add_function_bytecode" "add_function_native"

external add_function_export: m -> string -> string -> export = "add_function_export"

external add_global: m -> string -> ty -> bool -> exp -> global = "add_global"

external make_exp_global_get: m -> string -> ty -> exp = "make_exp_global_get"

external make_exp_global_set: m -> string -> exp -> exp = "make_exp_global_set"

external set_memory:
  m -> int -> int -> string -> string array -> bool array -> exp array -> int array -> bool -> unit =
  "set_memory_bytecode" "set_memory"
