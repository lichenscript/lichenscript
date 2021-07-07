
let init_object_fun_name = "__wtl_init_object"
let free_object_fun_name = "__wtl_free_object"
let free_object_var_name = "__wtl_free_obj"
let free_space_var_name = "__wtf_free_space"

(**
  * global info
  *
  * | __wtl_free_obj | ptr |
  *)
let codegen_allocator_facility (env: Codegen_env.t) =
  let module Dsl =
    Dsl.Binaryen(struct
      let m = env.module_
    end)
  in
  let open Dsl in
  let ptr_ty = Codegen_env.ptr_ty env in

  let wtf_alloc_fun_name = "__wtf_alloc_fun" in

  (* function __wtf_alloc_fun(size: i32) -> ptr *)
  (* local 1: tmp result *)
  let codegen_wtl_alloc () =
    let content =
      if_ (global_get free_object_var_name ptr_ty)  (* check free objects *)
        (block [|
          local_set 1 (global_get free_object_var_name ptr_ty);
          local_get 1 ptr_ty;
        |] ptr_ty)
        (block [|
          local_set 1 (global_get free_space_var_name ptr_ty);

          global_set free_space_var_name (binary add_i32 (local_get 1 ptr_ty) (local_get 0 i32));

          local_get 1 ptr_ty
        |] ptr_ty)
    in
    let params_ty = C_bindings.make_ty_multiples [| Codegen_env.ptr_ty env |] in
    let _ = function_ ~name:wtf_alloc_fun_name ~params_ty ~ret_ty:none ~vars_ty:[||] ~content in
    ()
  in

  (* function __wtl_init_object(obj: ptr) *)
  let codegen_wtl_init_object () =
    let content =
      memory_fill ~dest:(local_get 0 ptr_ty) ~value:(const_i32_of_int 0) ~size:(const_i32_of_int 16)
    in
    let params_ty = C_bindings.make_ty_multiples [| Codegen_env.ptr_ty env |] in
    let _ = function_ ~name:init_object_fun_name ~params_ty ~ret_ty:none ~vars_ty:[||] ~content in
    let _ = export_function init_object_fun_name init_object_fun_name in
    ()
  in

  (* function __wtl_free_object(obj: ptr) *)
  let codegen_wtl_free_object () =
    let content =
      block [|
        (* obj->next_free_obj = $global_free_obj *)
        store ~bytes:4 ~offset:8 ~align:0 ~ptr:(local_get 0 ptr_ty) ~value:(global_get free_object_var_name ptr_ty) ~ty:ptr_ty;

        (* global_free_obj <- obj *)
        global_set free_object_var_name (local_get 0 ptr_ty);

      |]
      auto
    in
    let params_ty = C_bindings.make_ty_multiples [| Codegen_env.ptr_ty env |] in
    let _ = function_ ~name:free_object_fun_name ~params_ty ~ret_ty:none ~vars_ty:[||] ~content in
    let _ = export_function free_object_fun_name free_object_fun_name in
    ()
  in

  let _ = add_global_var ~name:free_object_var_name ptr_ty ~mut:true ~init:(const_i32_of_int 0) in
  let _ = add_global_var ~name:free_space_var_name ptr_ty ~mut:true
    ~init:(const_i32_of_int env.config.stack_size)
  in

  codegen_wtl_alloc();
  codegen_wtl_init_object();
  codegen_wtl_free_object()
