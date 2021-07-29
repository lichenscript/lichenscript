open Ctypes
open Core

module Bindings (F : Cstubs.FOREIGN) = struct

  let module_ = ptr void

  let binary_type = uintptr_t

  let binary_op = int32_t

  let module_create = F.foreign "BinaryenModuleCreate" F.(void @-> returning module_)

  let module_dispose = F.foreign "BinaryenModuleDispose" F.(module_ @-> returning void)

  let type_none = F.foreign "BinaryenTypeNone" F.(void @-> returning binary_type)

  let type_int32 = F.foreign "BinaryenTypeInt32" F.(void @-> returning binary_type)

  let type_int64 = F.foreign "BinaryenTypeInt64" F.(void @-> returning binary_type)

  let type_f32 = F.foreign "BinaryenTypeFloat32" F.(void @-> returning binary_type)

  let type_f64 = F.foreign "BinaryenTypeFloat64" F.(void @-> returning binary_type)

  let type_any_ref = F.foreign "BinaryenTypeAnyref" F.(void @-> returning binary_type)

  let type_unreachable = F.foreign "BinaryenTypeUnreachable" F.(void @-> returning binary_type)

  let type_auto = F.foreign "BinaryenTypeAuto" F.(void @-> returning binary_type)

  let type_multiples = F.foreign "BinaryenTypeAuto" F.((ptr binary_type) @-> uint32_t @-> returning binary_type)

  let add_i32 = F.foreign "BinaryenAddInt32" F.(void @-> returning binary_op)

  let sub_i32 = F.foreign "BinaryenSubInt32" F.(void @-> returning binary_op)

  let mul_i32 = F.foreign "BinaryenMulInt32" F.(void @-> returning binary_op)

  let div_si32 = F.foreign "BinaryenDivSInt32" F.(void @-> returning binary_op)

  let lt_si32 = F.foreign "BinaryenLtSInt32" F.(void @-> returning binary_op)

  let le_si32 = F.foreign "BinaryenLeSInt32" F.(void @-> returning binary_op)

  let gt_si32 = F.foreign "BinaryenGtSInt32" F.(void @-> returning binary_op)

  let ge_si32 = F.foreign "BinaryenGeSInt32" F.(void @-> returning binary_op)

  let eq_si32 = F.foreign "BinaryenEqInt32" F.(void @-> returning binary_op)

  let ne_i32 = F.foreign "BinaryenNeInt32" F.(void @-> returning binary_op)
  
end

let codegen =
  Cstubs.write_ml Format.str_formatter ~prefix:"binaryen_stub" (module Bindings);
  let ml_content = Format.flush_str_formatter () in
  Out_channel.write_all "b.ml" ~data:ml_content;
  Format.fprintf Format.str_formatter "#include <binaryen-c.h>\n";
  Cstubs.write_c Format.str_formatter ~prefix:"binaryen_stub" (module Bindings);
  let c_content = Format.flush_str_formatter () in
  Out_channel.write_all "b.c" ~data:c_content
