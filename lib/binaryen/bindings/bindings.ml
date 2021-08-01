open Ctypes

module Bindings (F : Cstubs.FOREIGN) = struct

  type module_ = unit ptr

  let module_ : module_ typ = ptr void

  type binary_type = Ctypes.Uintptr.t

  let binary_type = uintptr_t

  type binary_op = int32

  let binary_op = int32_t

  type literal_data

  let literal_data : literal_data union typ = union "BinaryenLiteralData";;

  let literal_bytes_data = field literal_data "i32" int32_t;;

  let literal_data_v128_ty = array 16 uint8_t;;

  let literal_data_v128 = field literal_data "v128" literal_data_v128_ty;;

  seal literal_data;;

  type literal

  let literal : literal structure typ = structure "BinaryenLiteral";;

  let literal_type = field literal "type" uintptr_t;;

  let liteal_data = field literal "data" literal_data;;

  seal literal;;

  type binary_result

  let binary_result : binary_result structure typ = structure "BinaryenModuleAllocateAndWriteResult";;

  let binary_result_binary = field binary_result "binary" (ptr void);;

  let binary_result_size = field binary_result "binaryBytes" (size_t);;

  let binary_result_sourcemap = field binary_result "sourceMap" (ptr char);;

  seal binary_result;;

  type expression = unit ptr

  let expression : expression typ = ptr void

  type function_ = unit ptr

  let function_ : function_ typ = ptr void

  type export = unit ptr

  let export : export typ = ptr void

  type global_var = unit ptr

  let global_var : export typ = ptr void

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

  let type_multiples = F.foreign "BinaryenTypeCreate" F.((ptr binary_type) @-> uint32_t @-> returning binary_type)

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

  let literal_int32 = F.foreign "BinaryenLiteralInt32" F.(int32_t @-> returning literal)

  let literal_int64 = F.foreign "BinaryenLiteralInt64" F.(int64_t @-> returning literal)

  let literal_f32 = F.foreign "BinaryenLiteralFloat32" F.(Ctypes.float @-> returning literal)

  let literal_f64 = F.foreign "BinaryenLiteralFloat64" F.(double @-> returning literal)

  let expr_block = F.foreign "BinaryenBlock"
    F.(module_ @-> ptr char @-> ptr expression @-> size_t @-> binary_type @-> returning expression)

  let expr_const = F.foreign "BinaryenConst" F.(module_ @-> literal @-> returning expression)

  let expr_binary = F.foreign "BinaryenBinary" F.(module_ @-> binary_op @-> expression @-> expression @-> returning expression)

  let expr_unreachable = F.foreign "BinaryenUnreachable" F.(module_ @-> returning expression)

  let expr_return = F.foreign "BinaryenReturn" F.(module_ @-> expression @-> returning expression)

  let expr_if = F.foreign "BinaryenIf" F.(module_ @-> expression @-> expression @-> expression @-> returning expression)

  let expr_loop = F.foreign "BinaryenLoop" F.(module_ @-> string @-> expression @-> returning expression)

  let expr_break = F.foreign "BinaryenBreak" F.(module_ @-> string @-> expression @-> expression @-> returning expression)

  let expr_local_get = F.foreign "BinaryenLocalGet" F.(module_ @-> int @-> binary_type @-> returning expression)

  let expr_local_set = F.foreign "BinaryenLocalSet" F.(module_ @-> int @-> expression @-> returning expression)

  (* module -> bytes -> signed -> offset -> align -> type -> ptr -> exp *)
  let expr_load = F.foreign "BinaryenLoad"
    F.(module_ @-> int @-> bool @-> int @-> int @-> binary_type @-> expression @-> returning expression)

  let expr_store = F.foreign "BinaryenStore"
    F.(module_ @-> int @-> int @-> int @-> expression @-> expression @-> binary_type @-> returning expression)

  let expr_call = F.foreign "BinaryenCall"
    F.(module_ @-> string @-> ptr expression @-> size_t @-> binary_type @-> returning expression)

  let expr_memory_fill = F.foreign "BinaryenMemoryFill"
    F.(module_ @-> expression @-> expression @-> expression @-> returning expression)

  let expr_memory_copy = F.foreign "BinaryenMemoryCopy"
    F.(module_ @-> expression @-> expression @-> expression @-> returning expression)

  let add_function = F.foreign "BinaryenAddFunction"
    F.(module_ @-> string @-> binary_type @-> binary_type @-> ptr binary_type @-> size_t @-> expression @-> returning function_)

  let add_function_import = F.foreign "BinaryenAddFunctionImport"
    F.(module_ @-> string @-> string @-> string @-> binary_type @-> binary_type @-> returning void)

  let add_function_export = F.foreign "BinaryenAddFunctionExport"
    F.(module_ @-> string @-> string @-> returning export)

  let add_global = F.foreign "BinaryenAddGlobal"
    F.(module_ @-> string @-> binary_type @-> bool @-> expression @-> returning global_var)

  let expr_global_get = F.foreign "BinaryenGlobalGet"
    F.(module_ @-> string @-> binary_type @-> returning expression)

  let expr_global_set = F.foreign "BinaryenGlobalSet"
    F.(module_ @-> string @-> expression @-> returning expression)

  let set_memory = F.foreign "BinaryenSetMemory"
    F.(module_ @-> int @-> int @-> string @-> ptr string @-> ptr bool @-> ptr expression @-> ptr uint32_t @-> size_t @-> bool @-> returning void)

  let set_debug_info = F.foreign "BinaryenSetDebugInfo"
    F.(bool @-> returning void)

  let emit_text = F.foreign "BinaryenModuleAllocateAndWriteText"
    F.(module_ @-> returning string)

  let emit_binary = F.foreign "BinaryenModuleAllocateAndWrite"
    F.(module_ @-> string @-> returning binary_result)

  let clean_binary_result = F.foreign "clean_binary_result"
    F.(binary_result @-> returning void)
  
end
