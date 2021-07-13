open Binaryen

let next_align_size_fun_name = "__wtf_next_align_size"
let init_object_fun_name = "__wtl_init_object"
let free_object_fun_name = "__wtl_free_object"
let free_object_var_name = "__wtl_free_obj"
let free_space_var_name = "__wtf_free_space"
let memory_copy_fun_name = "__wtl_memory_copy"
let memory_fill_fun_name = "__wtl_memory_fill"
let wtf_alloc_fun_name = "__wtf_alloc_fun"

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

  let wtf_alloc_from_free_space_fun_name = "__wtf_alloc_from_free_space" in

  let codegen_wtf_next_align_size () =
    (* function next_align_size(size: i32, align: i32): i32 *)
    (* (n + k - 1) / k * k *)
    let _ = def_function next_align_size_fun_name ~params:[| i32; i32 |] ~ret_ty:i32 (fun _ ->
      (binary mul_i32
        (binary div_i32
          (binary sub_i32
            (binary add_i32 (I32.local_get 0) (I32.local_get 1))
            (const_i32_of_int 1))
          (I32.local_get 1))
          (I32.local_get 1))
    ) in
    ()
  in

  let codegen_wtl_alloc_from_free_space () =

    (* function __wtf_alloc_from_free_space(size: i32) *)
    (* local 1: really used_space *)
    (* local 2: tmp_result *)
    let _ = def_function wtf_alloc_from_free_space_fun_name ~params:[| i32 |] ~ret_ty:ptr_ty (fun ctx ->
      let really_used_space = def_local ctx i32 in
      let tmp_result = def_local ctx ptr_ty in
      
      (block ~ty:ptr_ty [|
        local_set really_used_space (call_ next_align_size_fun_name [| (I32.local_get 0); (const_i32_of_int 4) |] i32);

        local_set tmp_result (Ptr.global_get free_space_var_name);

        (* $free_space <- tmp_result + really_used_space *)
        global_set free_space_var_name (binary add_i32 (Ptr.local_get tmp_result) (I32.local_get really_used_space));

        (* result->used_space <- really_used_space *)
        I32.store ~offset:0 ~ptr:(Ptr.local_get tmp_result) (I32.local_get really_used_space);

        Ptr.local_get tmp_result
      |])
    )
    in ()
  in

  (* function __wtf_alloc_fun(size: i32) -> ptr *)
  (* local 1: tmp result *)
  (* local 2: obj total size *)
  (* local 3: current free_object *)
  let codegen_wtl_alloc () =
    let _ = def_function wtf_alloc_fun_name ~params:[| ptr_ty |] ~ret_ty:ptr_ty (fun ctx ->
        let tmp_result = def_local ctx ptr_ty in
        let remain_size = def_local ctx i32 in
        let prev_free_obj = def_local ctx ptr_ty in
        let current_free_obj = def_local ctx ptr_ty in

        block ~ty:ptr_ty ~name:"fun_blk" [|
          (* Question: Is this neccessary? *)
          local_set prev_free_obj (const_i32_of_int 0);

          (* current_free_obj <- $__wtf_free_obj *)
          local_set current_free_obj (Ptr.global_get free_object_var_name);

          loop "find_free_obj" (
            if_ (Ptr.local_get current_free_obj)  (* check free objects *)
              (block ~ty:none [|
                (* total_size <- (current_free_obj->total_bytes) *)
                local_set
                  remain_size
                  (I32.load ~offset:0
                    (Ptr.local_get current_free_obj));

                (* if (remain_size > required_size) *)
                (if_ (binary gt_i32 (I32.local_get remain_size) (I32.local_get 0))
                  (* return current free_object *)
                  (block ~ty:none [|
                    (* tmp_result <- $__wtl_free_obj *)
                    local_set tmp_result (Ptr.global_get free_object_var_name);

                    (if_ (Ptr.local_get prev_free_obj)
                      (* has prev free object *)
                      (* prev_free_obj->next <- current_free_obj->next *)
                      (Ptr.store ~offset:8 ~ptr:(Ptr.local_get prev_free_obj)
                        (Ptr.load ~offset:8 (Ptr.local_get current_free_obj)))

                      (* else *)
                      (* no prev object *)
                      (* $__wtl_free_obj <- (ptr->next_free_obj) *)
                      (global_set
                        free_object_var_name
                        (Ptr.load ~offset:8 (Ptr.local_get 0))));

                    (* return tmp result *)
                    break_ "fun_blk" ~value:(Ptr.local_get tmp_result);

                  |])
                  (* else *)
                  (* check next node on linked-list *)
                  (block ~ty:none [|

                    (* prev_free_obj <- curent_free_obj *)
                    local_set prev_free_obj (Ptr.local_get current_free_obj);

                    (* current_free_obj <- current_free_obj->next *)
                    local_set current_free_obj (Ptr.load ~offset:8 (Ptr.local_get current_free_obj));

                  |]));

              |])

              (* no suitable free obj found, alloca from free space *)
              (block [|
                local_set tmp_result (call_ wtf_alloc_from_free_space_fun_name [| Ptr.local_get 0 |] ptr_ty);
                break_ "fun_blk" ~value:(Ptr.local_get tmp_result);
              |])
          );

          Ptr.local_get tmp_result;
        |]
      )
    in
    ()
  in

  (* function __wtl_init_object(obj: ptr) *)
  let codegen_wtl_init_object () =
    let _ = def_function init_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun _ ->
        (* memory_fill ~dest:(Ptr.local_get 0) ~value:(const_i32_of_int 0) ~size:(const_i32_of_int 16) *)
        call_ memory_fill_fun_name [| Ptr.local_get 0; const_i32_of_int 0; const_i32_of_int 16 |] none
      )
    in
    let _ = export_function init_object_fun_name init_object_fun_name in
    ()
  in

  (* function __wtl_free_object(obj: ptr) *)
  let codegen_wtl_free_object () =
    let _ = def_function free_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun _ ->
        (block [|
          (* obj->next_free_obj = $global_free_obj *)
          Ptr.store ~offset:8 ~ptr:(Ptr.local_get 0) (Ptr.global_get free_object_var_name);

          (* global_free_obj <- obj *)
          global_set free_object_var_name (Ptr.local_get 0);

        |])
      )
    in
    let _ = export_function free_object_fun_name free_object_fun_name in ()
  in

  let _ = add_global_var ~name:free_object_var_name ptr_ty ~mut:true ~init:(const_i32_of_int 0) in
  let _ = add_global_var ~name:free_space_var_name ptr_ty ~mut:true
    ~init:(const_i32_of_int env.config.stack_size)
  in

  let codegen_memory_copy () =
    import_function
      ~intern_name:memory_copy_fun_name
      ~extern_name:"env"
      ~extern_base_name:"memory_copy"
      ~params_ty:(C_bindings.make_ty_multiples [| ptr_ty; ptr_ty; i32 |])
      ~ret_ty:none
  in

  let codegen_memory_fill () =
    import_function
      ~intern_name:memory_fill_fun_name
      ~extern_name:"env"
      ~extern_base_name:"memory_fill"
      ~params_ty:(C_bindings.make_ty_multiples [| ptr_ty; i32; i32 |])
      ~ret_ty:none
  in

  Codegen_env.add_js_snippet env {
    js_fun_def = "
    function memory_fill(dest, value, size) {
      for (var i = 0; i < size; i++) {
        HEAPU8[dest + i] = value;
      }
    }";
    js_add_env_def =
      Some "env['memory_fill'] = memory_fill;";
  };

  Codegen_env.add_js_snippet env {
    js_fun_def = "
    function memory_copy(dest, src, num) {
      HEAPU8.copyWithin(dest, src, src + num);
    }";
    js_add_env_def =
      Some "env['memory_copy'] = memory_copy;";
  };

  codegen_memory_copy();
  codegen_memory_fill();
  codegen_wtf_next_align_size();
  codegen_wtl_alloc_from_free_space();
  codegen_wtl_alloc();
  codegen_wtl_init_object();
  codegen_wtl_free_object()
