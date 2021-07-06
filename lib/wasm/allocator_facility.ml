
let init_object_fun_name = "__init_wtl_object"

(**
  * global info
  *
  * | allocated size         | i32 |
  * | remain size            | i32 |
  * | small area linked list | i32 |
  * | large area linked list | i32 |
  *)
let codegen_allocator_facility (env: Codegen_env.t) =
  let module Dsl =
    Dsl.Binaryen(struct
      let m = env.module_
    end)
  in
  let open Dsl in
  let ptr_ty = Codegen_env.ptr_ty env in
  let content =
    memory_fill ~dest:(local_get 0 ptr_ty) ~value:(const_i32_of_int 0) ~size:(const_i32_of_int 16)
  in
  let params_ty = C_bindings.make_ty_multiples [| Codegen_env.ptr_ty env |] in
  let _ = function_ ~name:init_object_fun_name ~params_ty ~ret_ty:none ~vars_ty:[||] ~content in
  let _ = export_function init_object_fun_name init_object_fun_name in
  ()
