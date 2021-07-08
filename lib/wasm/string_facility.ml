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

  (* function __init_string_fun_name(obj: ptr, static_str: ptr, size: i32) *)
  (* local 3: ptr_ty bytes offset *)
  let codegen_init_string_static () =
    let length_offset = 16 in
    let bytes_offset = 24 in
    let _ = def_function init_string_fun_name_static ~params:[| ptr_ty; ptr_ty; i32 |] ~ret_ty:none (fun ctx ->
        let offset = def_local ctx ptr_ty in
        block [|
          call_
            Allocator_facility.init_object_fun_name
            [| Ptr.local_get 0 |]
            none
          ;

          I32.store ~offset:length_offset ~ptr:(Ptr.local_get 0) (I32.local_get 2);

          local_set offset (binary add_i32 (Ptr.local_get 0) (const_i32_of_int bytes_offset));

          memory_copy ~dest:(Ptr.local_get offset) ~src:(Ptr.local_get 1) ~size:(I32.local_get 2);

        |]
      )
    in
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
