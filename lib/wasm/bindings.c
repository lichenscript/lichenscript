
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <binaryen-c.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>

#define TY_HANDLER(NAME) \
  /* Create an OCaml value encapsulating the pointer p */ \
  static value val_of_##NAME(NAME p) \
  { \
    value v = caml_alloc(1, Abstract_tag); \
    *((NAME*) Data_abstract_val(v)) = p; \
    return v; \
  } \
  /* Extract the pointer encapsulated in the given OCaml value */ \
  static NAME NAME##_of_val(value v) \
  { \
    return *((NAME*) Data_abstract_val(v)); \
  }

TY_HANDLER(BinaryenExpressionRef)
TY_HANDLER(BinaryenFunctionRef)

static struct custom_operations literal_ops = {
  "binaryen.literal",
  custom_finalize_default,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

static void finalize_module(value v) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(v);
  BinaryenModuleDispose(*ref);
}

static struct custom_operations module_ops = {
  "binaryen.module",
  finalize_module,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

CAMLprim value make_module() {
  value v = caml_alloc_custom(&literal_ops, sizeof(struct BinaryenLiteral*), 0, 1);
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(v);
  *ref = BinaryenModuleCreate();
  return v;
}

CAMLprim value module_emit(value v) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(v);
  static char buffer[4096];
  memset(buffer, 0, 4096);
  size_t size = BinaryenModuleWriteText(*ref, buffer, 4096);
  return caml_copy_string(buffer);
}

CAMLprim value make_ty_none() {
  BinaryenType t = BinaryenTypeNone();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_int32() {
  BinaryenType t = BinaryenTypeInt32();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_int64() {
  BinaryenType t = BinaryenTypeInt64();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_f32() {
  BinaryenType t = BinaryenTypeFloat32();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_f64() {
  BinaryenType t = BinaryenTypeFloat64();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_any_ref() {
  BinaryenType t = BinaryenTypeAnyref();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_unreachable() {
  BinaryenType t = BinaryenTypeUnreachable();
  return caml_copy_int64(t);
}

CAMLprim value make_op_add_i32() {
  BinaryenOp op = BinaryenAddInt32();
  return caml_copy_int32(op);
}

CAMLprim value make_op_sub_i32() {
  BinaryenOp op = BinaryenSubInt32();
  return caml_copy_int32(op);
}

CAMLprim value make_op_mul_i32() {
  BinaryenOp op = BinaryenMulInt32();
  return caml_copy_int32(op);
}

CAMLprim value make_literal_i32(value i) {
  value v = caml_alloc_custom(&literal_ops, sizeof(struct BinaryenLiteral), 0, 1);
  int32_t int_val = Int32_val(i);
  struct BinaryenLiteral* alloc_v = (struct BinaryenLiteral*)Data_custom_val(v);
  *alloc_v = BinaryenLiteralInt32(int_val);
  return v;
}

CAMLprim value make_literal_i64(value i) {
  value v = caml_alloc_custom(&literal_ops, sizeof(struct BinaryenLiteral), 0, 1);
  int64_t int_val = Int64_val(i);
  struct BinaryenLiteral* alloc_v = (struct BinaryenLiteral*)Data_custom_val(v);
  *alloc_v = BinaryenLiteralInt64(int_val);
  return v;
}

CAMLprim value make_literal_f32(value i) {
  value v = caml_alloc_custom(&literal_ops, sizeof(struct BinaryenLiteral), 0, 1);
  double double_val = Double_val(i);
  struct BinaryenLiteral* alloc_v = (struct BinaryenLiteral*)Data_custom_val(v);
  *alloc_v = BinaryenLiteralFloat32((float)double_val);
  return v;
}

CAMLprim value make_literal_f64(value i) {
  value v = caml_alloc_custom(&literal_ops, sizeof(struct BinaryenLiteral), 0, 1);
  double double_val = Double_val(i);
  struct BinaryenLiteral* alloc_v = (struct BinaryenLiteral*)Data_custom_val(v);
  *alloc_v = BinaryenLiteralFloat64(double_val);
  return v;
}

CAMLprim value make_exp_const(value module, value literal) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  struct BinaryenLiteral* literal_st = (struct BinaryenLiteral*)Data_custom_val(literal);
  BinaryenExpressionRef exp = BinaryenConst(*ref, *literal_st);
  return val_of_BinaryenExpressionRef(exp);
}

CAMLprim value make_exp_binary(value module, value op, value left, value right) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  int32_t op_i = Int32_val(op);
  BinaryenExpressionRef left_exp = BinaryenExpressionRef_of_val(left);
  BinaryenExpressionRef right_exp = BinaryenExpressionRef_of_val(right);
  BinaryenExpressionRef bin = BinaryenBinary(*ref, op_i, left_exp, right_exp);
  return val_of_BinaryenExpressionRef(bin);
}

CAMLprim value add_function_native(value module, value name, value params_type, value result_ty, value var_types, value body) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* fun_name = String_val(name);
  int64_t params_type_i = Int64_val(params_type);
  int64_t result_type_i = Int64_val(result_ty);

  mlsize_t types_len = caml_array_length(var_types);

  BinaryenType* types = NULL;

  if (types_len != 0) {
    types = (BinaryenType*)alloca(sizeof(BinaryenType) * types_len);
    for (mlsize_t i = 0; i < types_len; i++) {
      value item = Field(var_types, i);
      types[i] = Int64_val(item);
    }
  }

  BinaryenExpressionRef exp = BinaryenExpressionRef_of_val(body);

  BinaryenFunctionRef fun_ref = BinaryenAddFunction(*ref, fun_name, params_type_i, result_type_i, types, types_len, exp);
  return val_of_BinaryenFunctionRef(fun_ref);
}

CAMLprim value add_function_bytecode(value * argv, int argn) {
  return add_function_native(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}
