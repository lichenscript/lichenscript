
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <binaryen-c.h>
#define CAML_NAME_SPACE
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

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
TY_HANDLER(BinaryenExportRef)
TY_HANDLER(BinaryenGlobalRef)

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

CAMLprim value module_emit_text(value v) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(v);
  char* result_str = BinaryenModuleAllocateAndWriteText(*ref);
  value result = caml_copy_string(result_str);
  free(result_str);
  return result;
}

static void clean_binary_result(BinaryenModuleAllocateAndWriteResult result) {
  if (result.binary) {
    free(result.binary);
  }
  if (result.sourceMap) {
    free(result.sourceMap);
  }
}

CAMLprim value module_emit_binary(value module, value path) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* path_str = String_val(path);
  int fd;

  fd = open(path_str, O_RDWR | O_CREAT | O_TRUNC, 0666);
  if (fd < 0) {
    caml_invalid_argument("can not open file");
    return Val_unit;
  }

  BinaryenModuleAllocateAndWriteResult binary_result = BinaryenModuleAllocateAndWrite(*ref, "");

  ftruncate(fd, binary_result.binaryBytes);

  char* src = (char*)mmap(NULL, binary_result.binaryBytes, PROT_READ | PROT_WRITE,
    MAP_SHARED, fd, 0);
  if (src < 0 || src == 0) {
    clean_binary_result(binary_result);
    close(fd);
    caml_failwith("map to file failed");
    return Val_unit;
  }

  memcpy(src, binary_result.binary, binary_result.binaryBytes);

  munmap(src, binary_result.binaryBytes);

  close(fd);

  clean_binary_result(binary_result);

  return Val_unit;
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

CAMLprim value make_ty_auto() {
  BinaryenType t = BinaryenTypeAuto();
  return caml_copy_int64(t);
}

CAMLprim value make_ty_multiples(value arr) {
  mlsize_t size = caml_array_length(arr);
  BinaryenType* types = alloca(sizeof(BinaryenType) * size);

  for (mlsize_t i = 0; i < size; i++) {
    BinaryenType t = Int64_val(Field(arr, i));
    types[i] = t;
  }

  BinaryenType result = BinaryenTypeCreate(types, size);
  return caml_copy_int64(result);
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

CAMLprim value make_exp_block(value module, value name, value children, value ty) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* name_str = NULL;

  if (Is_some(name)) {
    name_str = String_val(Some_val(name));
  }
  
  mlsize_t children_size = caml_array_length(children);

  BinaryenExpressionRef* children_exprs = NULL;
  children_exprs = (BinaryenExpressionRef*)alloca(sizeof(BinaryenExpressionRef) * children_size);

  for (mlsize_t i = 0; i < children_size; i++) {
    children_exprs[i] = BinaryenExpressionRef_of_val(Field(children, i));
  }

  BinaryenType t = Int64_val(ty);

  BinaryenExpressionRef result = BinaryenBlock(*ref, name_str, children_exprs, children_size, t);
  return val_of_BinaryenExpressionRef(result);
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

CAMLprim value make_exp_unreachable(value module) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  BinaryenExpressionRef u = BinaryenUnreachable(*ref);
  return val_of_BinaryenExpressionRef(u);
}

CAMLprim value make_exp_return(value module, value exp) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  BinaryenExpressionRef exp_ref = NULL;
  if (Is_some(exp)) {
    exp_ref = BinaryenExpressionRef_of_val(Some_val(exp));
  }
  BinaryenExpressionRef result = BinaryenReturn(*ref, exp_ref);
  return val_of_BinaryenExpressionRef(result);
}

CAMLprim value make_exp_local_get(value module, value index, value ty) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  int index_i = Int_val(index);
  BinaryenType ty_v = Int64_val(ty);
  BinaryenExpressionRef result = BinaryenLocalGet(*ref, index_i, ty_v);
  return val_of_BinaryenExpressionRef(result);
}

CAMLprim value make_exp_local_set(value module, value index, value exp) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  int index_i = Int_val(index);
  BinaryenExpressionRef expr = BinaryenExpressionRef_of_val(exp);
  BinaryenExpressionRef result = BinaryenLocalSet(*ref, index_i, expr);
  return val_of_BinaryenExpressionRef(result);
}

CAMLprim value make_exp_store(value module, value bytes_i, value offset_i, value align_i, value ptr, value content, value ty) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  int bytes = Int_val(bytes_i);
  int offset = Int_val(offset_i);
  int align = Int_val(align_i);
  BinaryenExpressionRef ptr_ref = BinaryenExpressionRef_of_val(ptr);
  BinaryenExpressionRef content_ref = BinaryenExpressionRef_of_val(content);
  BinaryenType v_ty = Int64_val(ty);

  BinaryenExpressionRef result = BinaryenStore(*ref, bytes, offset, align, ptr_ref, content_ref, v_ty);
  return val_of_BinaryenExpressionRef(result);
}

CAMLprim value make_exp_store_bytecode(value * argv, int argn) {
  return make_exp_store(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5], argv[6]);
}

CAMLprim value make_exp_call(value module, value name, value params, value return_type) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* name_str = String_val(name);

  mlsize_t params_len = caml_array_length(params);

  BinaryenExpressionRef* params_arr = NULL;

  if (params_len != 0) {
    params_arr = (BinaryenExpressionRef*)alloca(sizeof(BinaryenExpressionRef) * params_len);
    for (mlsize_t i = 0; i < params_len; i++) {
      value param = Field(params, i);
      params_arr[i] = BinaryenExpressionRef_of_val(param);
    }
  }

  BinaryenType ret_ty = Int64_val(return_type);

  BinaryenExpressionRef result = BinaryenCall(*ref, name_str, params_arr, params_len, ret_ty);
  return val_of_BinaryenExpressionRef(result);
}

CAMLprim value make_exp_memory_fill(value module, value dest_exp, value value_exp, value size_exp) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);

  BinaryenExpressionRef dest = BinaryenExpressionRef_of_val(dest_exp);
  BinaryenExpressionRef value = BinaryenExpressionRef_of_val(value_exp);
  BinaryenExpressionRef size = BinaryenExpressionRef_of_val(size_exp);

  BinaryenExpressionRef result = BinaryenMemoryFill(*ref, dest, value, size);
  return val_of_BinaryenExpressionRef(result);
}

CAMLprim value make_exp_memory_copy(value module, value dest_exp, value value_exp, value size_exp) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);

  BinaryenExpressionRef dest = BinaryenExpressionRef_of_val(dest_exp);
  BinaryenExpressionRef src = BinaryenExpressionRef_of_val(value_exp);
  BinaryenExpressionRef size = BinaryenExpressionRef_of_val(size_exp);

  BinaryenExpressionRef result = BinaryenMemoryCopy(*ref, dest, src, size);
  return val_of_BinaryenExpressionRef(result);
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

CAMLprim value add_function_import(value module, value intern_name_str, value extern_name_str, value extern_name_base_str, value params_ty_i, value ret_ty_i) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* intern_name = String_val(intern_name_str);
  const char* extern_name = String_val(extern_name_str);
  const char* extern_name_base = String_val(extern_name_base_str);
  BinaryenType params_ty = Int64_val(params_ty_i);
  BinaryenType ret_ty = Int64_val(ret_ty_i);
  BinaryenAddFunctionImport(*ref, intern_name, extern_name, extern_name_base, params_ty, ret_ty);
  return Val_none;
}

CAMLprim value add_function_import_bytecode(value * argv, int argn) {
  return add_function_import(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5]);
}

CAMLprim value add_function_export(value module, value intern_name, value extern_name) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* intern_name_c = String_val(intern_name);
  const char* extern_name_c = String_val(extern_name);
  BinaryenExportRef export = BinaryenAddFunctionExport(*ref, intern_name_c, extern_name_c);
  return val_of_BinaryenExportRef(export);
}

CAMLprim value add_global(value module, value name, value type, value mut, value init_expr) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* global_name = String_val(name);
  BinaryenType ty = Int64_val(type);
  bool is_mutable = Bool_val(mut);
  BinaryenExpressionRef exp = BinaryenExpressionRef_of_val(init_expr);
  BinaryenGlobalRef global = BinaryenAddGlobal(*ref, global_name, ty, is_mutable, exp);
  return val_of_BinaryenGlobalRef(global);
}

CAMLprim value make_exp_global_get(value module, value name, value type) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* global_name = String_val(name);
  BinaryenType ty = Int64_val(type);
  BinaryenExpressionRef exp = BinaryenGlobalGet(*ref, global_name, ty);
  return val_of_BinaryenExpressionRef(exp);
}

CAMLprim value make_exp_global_set(value module, value name, value v) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  const char* global_name = String_val(name);
  BinaryenExpressionRef set_value = BinaryenExpressionRef_of_val(v);
  BinaryenExpressionRef exp = BinaryenGlobalSet(*ref, global_name, set_value);
  return val_of_BinaryenExpressionRef(exp);
}

CAMLprim value set_memory(value module, value initial_int, value max_int, value export_name_str, value segments, value segmentPassive, value segmentOffsets, value shared_bl) {
  BinaryenModuleRef* ref = (BinaryenModuleRef*)Data_custom_val(module);
  int initial = Int_val(initial_int);
  int maximum = Int_val(max_int);
  const char* export_name = String_val(export_name_str);

  mlsize_t types_len = caml_array_length(segments);
  mlsize_t segments_len = types_len;

  const char** segments_c = NULL;
  BinaryenIndex* segments_size = NULL;

  if (types_len != 0) {
    segments_c = (const char**)alloca(sizeof(const char*) * types_len);
    segments_size = (BinaryenIndex*)alloca(sizeof(BinaryenIndex) * types_len);
  }

  for (mlsize_t i = 0; i < types_len; i++) {
    value tmp = Field(segments, i);
    segments_c[i] = String_val(tmp);
    segments_size[i] = strlen(segments_c[i]);
  }

  types_len = caml_array_length(segmentPassive);

  bool* segment_passive_arr = NULL;
  if (types_len != 0) {
    segment_passive_arr = (bool*)alloca(sizeof(bool) * types_len);
  }

  for (mlsize_t i = 0; i < types_len; i++) {
    segment_passive_arr[i] = Bool_val(Field(segmentPassive, i));
  }

  BinaryenExpressionRef* offsets = NULL;

  types_len = caml_array_length(segmentOffsets);

  if (types_len != 0) {
    offsets = (BinaryenExpressionRef*)alloca(sizeof(BinaryenExpressionRef) * types_len);
  }

  for (mlsize_t i = 0; i < types_len; i++) {
    offsets[i] = BinaryenExpressionRef_of_val(Field(segmentOffsets, i));
  }

  bool shared = Bool_val(shared_bl);
  BinaryenSetMemory(*ref, initial, maximum, export_name, segments_c, segment_passive_arr, offsets, segments_size, segments_len, shared);
  return Val_unit;
}

CAMLprim value set_memory_bytecode(value * argv, int argn) {
  return set_memory(argv[0], argv[1], argv[2], argv[3], argv[4], argv[5],
    argv[6], argv[7]);
}
