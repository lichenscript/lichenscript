open Binaryen

let init_string_fun_name = "__init_wtl_string"
let init_string_fun_name_static = "__init_wtl_string_static"

let codegen_string_facility (env: Codegen_env.t) =
  let module Dsl =
    Dsl.Binaryen(struct
      let ptr_ty = Codegen_env.ptr_ty env
      let m = env.module_
    end)
  in
  let open Dsl in

  (* function __init_string_fun_name(obj: ptr) *)
  let codegen_init_string () =
    let _ = def_function init_string_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun _ ->
        call_
          Allocator_facility.init_object_fun_name
          [| Ptr.local_get 0 |]
          none
      )
    in
    let _ = export_function init_string_fun_name init_string_fun_name in ()
  in

  (* function __init_string_fun_name(obj: ptr, static_str: ptr, str_size: i32) *)
  (* local 3: ptr_ty bytes offset *)
  let codegen_init_string_static () =
    let length_offset = 16 in
    let bytes_offset = 24 in
    let _ = def_function init_string_fun_name_static ~params:[| ptr_ty; i32 |] ~ret_ty:ptr_ty (fun ctx ->
        let new_ptr = def_local ctx ptr_ty in
        let str_bytes_size = def_local ctx i32 in
        let need_size = def_local ctx i32 in
        let offset = def_local ctx ptr_ty in
        block ~ty:ptr_ty [|
          local_set str_bytes_size (binary add_i32 (binary add_i32 (I32.local_get 1) (I32.local_get 1)) (const_i32_of_int 2));

          local_set need_size (binary add_i32 (I32.local_get str_bytes_size) (const_i32_of_int bytes_offset));

          (* alloc new object *)
          local_set new_ptr (call_ Allocator_facility.wtf_alloc_fun_name [| Ptr.local_get need_size |] ptr_ty);

          call_
            Allocator_facility.init_object_fun_name
            [| Ptr.local_get new_ptr |]
            none
          ;

          I32.store ~offset:length_offset ~ptr:(Ptr.local_get new_ptr) (I32.local_get 1);

          local_set offset (binary add_i32 (Ptr.local_get new_ptr) (const_i32_of_int bytes_offset));

          call_ Allocator_facility.memory_copy_fun_name [| Ptr.local_get offset; Ptr.local_get 0; I32.local_get 1 |] none;
          (* memory_copy ~dest:(Ptr.local_get offset) ~src:(Ptr.local_get 1) ~size:(I32.local_get 2); *)

          Ptr.local_get new_ptr

        |]
      )
    in
    let _ = export_function init_string_fun_name_static init_string_fun_name_static in
    ()
  in

  let codegen_console_log () =
    import_function
      ~intern_name:"__wtl_console_log"
      ~extern_name:"env"
      ~extern_base_name:"console_log"
      ~params_ty:ptr_ty
      ~ret_ty:none
  in

  codegen_init_string();
  codegen_init_string_static();
  codegen_console_log ()
