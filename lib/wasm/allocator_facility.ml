
let codegen_allocator_facility (module M: Dsl.BinaryenModule) =
  let init_object_fun_name = "__init_waterlang_object" in
  let module Dsl = Dsl.Binaryen(M) in
  let open Dsl in
  let content =
    memory_fill ~dest:(local_get 0 i32) ~value:(const_i32 (Int32.of_int 0)) ~size:(const_i32 (Int32.of_int 16))
  in
  let params_ty = C_bindings.make_ty_multiples [| i32 |] in
  let _ = function_ ~name:init_object_fun_name ~params_ty ~ret_ty:none ~vars_ty:[||] ~content in
  let _ = export_function init_object_fun_name init_object_fun_name in
  ()
