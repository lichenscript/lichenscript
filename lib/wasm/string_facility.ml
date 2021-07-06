
let init_string_fun_name = "__init_wtl_string"
let init_string_fun_name_static = "__init_wtl_string_static"

let codegen_string_facility (env: Codegen_env.t) =
  let module Dsl =
    Dsl.Binaryen(struct
      let m = env.module_
    end)
  in
  let open Dsl in
  let ptr_ty = Codegen_env.ptr_ty env in

  (* function __init_string_fun_name(obj: ptr) *)
  let codegen_init_string () =
    let content =
      call_
        Allocator_facility.init_object_fun_name
        [| local_get 0 (Codegen_env.ptr_ty env) |]
        none
    in
    let params_ty = C_bindings.make_ty_multiples [| Codegen_env.ptr_ty env |] in
    let _ = function_ ~name:init_string_fun_name ~params_ty ~ret_ty:none ~vars_ty:[||] ~content in
    let _ = export_function init_string_fun_name init_string_fun_name in
    ()
  in

  (* function __init_string_fun_name(obj: ptr, static_str: ptr, size: i32) *)
  (* local 3: ptr_ty bytes offset *)
  let codegen_init_string_static () =
    let length_offset = 16 in
    let bytes_offset = 24 in
    let content =
      block [|
        call_
          Allocator_facility.init_object_fun_name
          [| local_get 0 (Codegen_env.ptr_ty env) |]
          none
        ;

        store ~bytes:4 ~offset:length_offset ~align:0
          ~ptr:(local_get 0 ptr_ty) ~value:(local_get 2 i32) ~ty:i32
        ;

        local_set 3 (binary add_i32 (local_get 0 ptr_ty) (const_i32_of_int bytes_offset));

        memory_copy ~dest:(local_get 3 ptr_ty) ~src:(local_get 1 ptr_ty) ~size:(local_get 2 i32);

      |]
      auto
    in
    let params_ty = C_bindings.make_ty_multiples [| Codegen_env.ptr_ty env |] in
    let _ = function_ ~name:init_string_fun_name_static ~params_ty ~ret_ty:none ~vars_ty:[| ptr_ty |] ~content in
    let _ = export_function init_string_fun_name_static init_string_fun_name_static in
    ()
  in

  let codegen_console_log () =
    import_function
      ~intern_name:"__wtl_console_log"
      ~extern_name:"console_log"
      ~extern_base_name:"env"
      ~params_ty:ptr_ty
      ~ret_ty:none
  in

  codegen_init_string();
  codegen_init_string_static();
  codegen_console_log ()
