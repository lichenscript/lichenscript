
open Ctypes
open Foreign

let malloc = foreign "malloc"
    (size_t @-> returning (ptr void))

let free = foreign "free"
    ((ptr void) @-> returning void)

let binaryen_index = uint32_t

module Literal = struct

  type t = unit
  type u = unit

  let t: t Ctypes_static.structure typ = structure "BinaryenLiteral"

  let u: u Ctypes_static.union typ = union "tmp"

  let u_i32 = field u "i32" int32_t

  let u_i64 = field u "i64" int64_t

  let u_f32 = field u "f32" float

  let u_f64 = field u "f64" double

  let v128_ty = array 16 uint8_t

  let u_v128 = field u "v128" v128_ty

  let u_func = field u "func" (ptr char)

  let () = seal u

  let lit_type = field t "type" uintptr_t
  let union_field = field t "u" u

  let () = seal t

  let mk_int32 = foreign "BinaryenLiteralInt32"
    (int32_t @-> returning t)

  let mk_int64 = foreign "BinaryenLiteralInt64"
    (int64_t @-> returning t)

  let mk_f32 = foreign "BinaryenLiteralFloat32"
    (float @-> returning t)

  let mk_f64 = foreign "BinaryenLiteralFloat64"
    (double @-> returning t)
  
end

module Module = struct
  type t = unit ptr

  let binary_module : t typ = ptr void

  let create = foreign "BinaryenModuleCreate" (void @-> returning binary_module)

  let dispose = foreign "BinaryenModuleDispose" (binary_module @-> returning void)

  let with_module callback =
    let m = create () in
    callback m;
    dispose m

  let write_text = foreign "BinaryenModuleWriteText"
    (binary_module @-> ptr char @-> size_t @-> returning size_t)

  let emit_test m =
    let buffer_size = 4096 in
    let raw_ptr = malloc (Unsigned.Size_t.of_int buffer_size) in
    let str_ptr = from_voidp char raw_ptr in
    let size = write_text m str_ptr (Unsigned.Size_t.of_int buffer_size) in
    let result = string_from_ptr str_ptr ~length:(Unsigned.Size_t.to_int size) in
    free raw_ptr;
    result
  
end

module Type = struct

  type t = Uintptr.t

  let none = foreign "BinaryenTypeNone"
    (void @-> returning uintptr_t)

  let int32 = foreign "BinaryenTypeInt32"
    (void @-> returning uintptr_t)

  let int64 = foreign "BinaryenTypeInt64"
    (void @-> returning uintptr_t)

  let f32 = foreign "BinaryenTypeFloat32"
    (void @-> returning uintptr_t)

  let f64 = foreign "BinaryenTypeFloat64"
    (void @-> returning uintptr_t)

  let any_ref = foreign "BinaryenTypeAnyref"
    (void @-> returning uintptr_t)

  let unreachable = foreign "BinaryenTypeUnreachable"
    (void @-> returning uintptr_t)
  
end

module Op = struct
  type t = int32

  let tval = Ctypes.int32_t

  let add_i32 =
    foreign "BinaryenAddInt32" (void @-> returning tval)

  let sub_i32 =
    foreign "BinaryenSubInt32" (void @-> returning tval)

  let mul_i32 =
    foreign "BinaryenMulInt32" (void @-> returning tval)
  
end

module Expr = struct
  type t = unit ptr

  let binary_expression : t typ = ptr void

  let wrapped_if =
    foreign "BinaryenIf" (Module.binary_module @-> binary_expression @-> binary_expression @-> binary_expression @-> returning binary_expression)

  let if' m ~condition ~t ~f =
    wrapped_if m condition t f

  let wrapped_binary = foreign "BinaryenBinary"
    (Module.binary_module @-> Op.tval @-> binary_expression @-> binary_expression @-> returning binary_expression)

  let const = foreign "BinaryenConst"
    (Module.binary_module @-> Literal.t @-> returning binary_expression)

  let binary m op ~left ~right =
    wrapped_binary m op left right
  
end

module Function = struct
  type t = unit ptr

  let tval = ptr void

  let wrapped_create = foreign "BinaryenAddFunction"
    (Module.binary_module @->
      string @->
      uintptr_t @->
      uintptr_t @->
      ptr uintptr_t @->
      binaryen_index @->
      Expr.binary_expression @->
      returning tval)

  let add m ~name ~params ~result ~(varTypes: Type.t list) ~(body: Expr.t) =
    let len = List.length varTypes in
    let var_types_ptr = allocate_n uintptr_t ~count:len in
    List.iteri (fun index item ->
        (var_types_ptr +@ index) <-@ item
      )
      varTypes
    ;
    wrapped_create m name params result var_types_ptr (Unsigned.UInt32.of_int len) body
  
end
