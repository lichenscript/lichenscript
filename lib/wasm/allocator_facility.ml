
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
      let ptr_ty = Codegen_env.ptr_ty env
      let m = env.module_
    end)
  in
  let open Dsl in

  let wtf_alloc_fun_name = "__wtf_alloc_fun" in
  let wtf_alloc_from_free_space_fun_name = "__wtf_alloc_from_free_space" in

  let codegen_wtl_alloc_from_free_space () =
    let _ = def_function wtf_alloc_from_free_space_fun_name ~params:[| i32 |] ~ret_ty:ptr_ty (fun ctx ->
      let tmp_result = def_local ctx ptr_ty in
      (block [|
        local_set tmp_result (Ptr.global_get free_space_var_name);

        global_set free_space_var_name (binary add_i32 (Ptr.local_get 1) (I32.local_get 0));

        Ptr.local_get tmp_result
      |] ptr_ty)
    )
    in ()
  in

  (* function __wtf_alloc_fun(size: i32) -> ptr *)
  (* local 1: tmp result *)
  (* local 2: obj total size *)
  let codegen_wtl_alloc () =
    let _ = def_function wtf_alloc_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun ctx ->
        let tmp_result = def_local ctx ptr_ty in
        let remain_size = def_local ctx i32 in

        loop "find_free_obj" (
          if_ (global_get free_object_var_name ptr_ty)  (* check free objects *)
            (block [|
              (* total_size <- (ptr->total_bytes) *)
              local_set
                remain_size
                (load ~bytes:4 ~signed:true ~offset:0 ~align:0 ~ty:i32
                  (global_get free_object_var_name ptr_ty));

              (* if (remain_size > required_size) *)
              (if_ (binary gt_i32 (I32.local_get remain_size) (I32.local_get 0))
                (* return current free_object *)
                (block [|
                  (* tmp_result <- $__wtl_free_obj *)
                  local_set tmp_result (Ptr.global_get free_object_var_name);

                  (* $__wtl_free_obj <- (ptr->next_free_obj) *)
                  global_set
                    free_object_var_name
                    (load
                      ~bytes:4 ~signed:true ~offset:8 ~align:0 ~ty:ptr_ty
                      (Ptr.local_get 0));

                  (* return tmp result *)
                  Ptr.local_get tmp_result;

                |] auto)
                (call_ wtf_alloc_from_free_space_fun_name [| Ptr.local_get 0 |] ptr_ty));

            |] ptr_ty)

            (* no free objs, alloca from free space *)
            (break_
              ?value:(Some(call_ wtf_alloc_from_free_space_fun_name [| Ptr.local_get 0 |] ptr_ty))
              "find_free_obj")
        )
      )
    in
    ()
  in

  (* function __wtl_init_object(obj: ptr) *)
  let codegen_wtl_init_object () =
    let _ = def_function init_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun _ ->
        memory_fill ~dest:(Ptr.local_get 0) ~value:(const_i32_of_int 0) ~size:(const_i32_of_int 16)
      )
    in
    let _ = export_function init_object_fun_name init_object_fun_name in
    ()
  in

  (* function __wtl_free_object(obj: ptr) *)
  let codegen_wtl_free_object () =
    let _ = def_function free_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun _ ->
        block [|
          (* obj->next_free_obj = $global_free_obj *)
          store ~bytes:4 ~offset:8 ~align:0 ~ptr:(Ptr.local_get 0) ~value:(Ptr.global_get free_object_var_name) ~ty:ptr_ty;

          (* global_free_obj <- obj *)
          global_set free_object_var_name (Ptr.local_get 0);

        |]
        auto
      )
    in
    let _ = export_function free_object_fun_name free_object_fun_name in ()
  in

  let _ = add_global_var ~name:free_object_var_name ptr_ty ~mut:true ~init:(const_i32_of_int 0) in
  let _ = add_global_var ~name:free_space_var_name ptr_ty ~mut:true
    ~init:(const_i32_of_int env.config.stack_size)
  in

  codegen_wtl_alloc_from_free_space();
  codegen_wtl_alloc();
  codegen_wtl_init_object();
  codegen_wtl_free_object()
