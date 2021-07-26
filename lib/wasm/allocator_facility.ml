open Binaryen

let next_align_size_fun_name = "__wtf_next_align_size"
let init_object_fun_name = "__wtl_init_object"
let free_object_fun_name = "__wtl_free_object"
let free_object_var_name = "__wtl_free_obj"
let free_space_var_name = "__wtf_free_space"
let memory_copy_fun_name = "__wtl_memory_copy"
let memory_fill_fun_name = "__wtl_memory_fill"
let wtf_alloc_fun_name = "__wtf_alloc"

let realloc_fun_name = "__wtf_realloc"
let retain_object_fun_name = "__wtf_retain_object"
let release_object_fun_name = "__wtf_release_object"

let codegen_memory_fill (env: Codegen_env.t) ~dest ~value ~size =
  let module Dsl =
    Dsl.Binaryen(struct
      let ptr_ty = Codegen_env.ptr_ty env
      let m = env.module_
    end)
  in
  let open Dsl in
  if env.config.memory_bulk_operations then
    memory_fill ~dest ~value ~size
  else
    call_ memory_fill_fun_name [| dest; value; size |] none

let codegen_memory_copy (env: Codegen_env.t) ~dest ~src ~size =
  let module Dsl =
    Dsl.Binaryen(struct
      let ptr_ty = Codegen_env.ptr_ty env
      let m = env.module_
    end)
  in
  let open Dsl in
  if env.config.memory_bulk_operations then
    memory_copy ~dest ~src ~size
  else
    call_ memory_copy_fun_name [| dest; src; size |] none

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
      I32.(
        ((I32.local_get 0) + (I32.local_get 1) - (const_i32_of_int 1)) / (I32.local_get 1) * (I32.local_get 1)
    )) in ()
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
    let _ = def_function wtf_alloc_fun_name ~params:[| i32 |] ~ret_ty:ptr_ty (fun ctx ->
        let tmp_result = def_local ctx ptr_ty in
        let remain_size = def_local ctx i32 in
        let prev_free_obj = def_local ctx ptr_ty in
        let current_free_obj = def_local ctx ptr_ty in

        block ~ty:ptr_ty ~name:"fun_blk" [|
          (* if (size == 0) return nullptr; *)
          if' Ptr.((local_get 0) == (const_i32_of_int 0))
            ~then':(break_ "fun_blk" ~value:(const_i32_of_int 0));

          (* Question: Is this neccessary? *)
          local_set prev_free_obj (const_i32_of_int 0);

          (* current_free_obj <- $__wtf_free_obj *)
          local_set current_free_obj (Ptr.global_get free_object_var_name);

          loop "find_free_obj" (
            if' (Ptr.local_get current_free_obj)  (* check free objects *)
              ~then':(block ~ty:none [|
                (* total_size <- (current_free_obj->total_bytes) *)
                local_set
                  remain_size
                  (I32.load ~offset:0
                    (Ptr.local_get current_free_obj));

                (* if (remain_size > required_size) *)
                (if' (binary gt_i32 (I32.local_get remain_size) (I32.local_get 0))
                  (* return current free_object *)
                  ~then':(block ~ty:none [|
                    (* tmp_result <- $__wtl_free_obj *)
                    local_set tmp_result (Ptr.global_get free_object_var_name);

                    (if' (Ptr.local_get prev_free_obj)
                      (* has prev free object *)
                      (* prev_free_obj->next <- current_free_obj->next *)
                      ~then':(Ptr.store ~offset:8 ~ptr:(Ptr.local_get prev_free_obj)
                        (Ptr.load ~offset:8 (Ptr.local_get current_free_obj)))

                      (* no prev object *)
                      (* $__wtl_free_obj <- (ptr->next_free_obj) *)
                      ~else':(global_set
                        free_object_var_name
                        (Ptr.load ~offset:8 (Ptr.local_get 0))));

                    (* return tmp result *)
                    break_ "fun_blk" ~value:(Ptr.local_get tmp_result);

                  |])
                  (* check next node on linked-list *)
                  ~else':(block ~ty:none [|

                    (* prev_free_obj <- curent_free_obj *)
                    local_set prev_free_obj (Ptr.local_get current_free_obj);

                    (* current_free_obj <- current_free_obj->next *)
                    local_set current_free_obj (Ptr.load ~offset:8 (Ptr.local_get current_free_obj));

                    break_ "find_free_obj";

                  |]));

              |])
              (* no suitable free obj found, alloca from free space *)
              ~else':(block [|
                local_set tmp_result (call_ wtf_alloc_from_free_space_fun_name [| Ptr.local_get 0 |] ptr_ty);
                break_ "fun_blk" ~value:(Ptr.local_get tmp_result);
              |])
          );

          Ptr.local_get tmp_result;
        |]
      )
    in ()
  in

  let codegen_wtf_realloc () =
    let _ = def_function realloc_fun_name ~params:[| ptr_ty; i32 |] ~ret_ty:ptr_ty (fun ctx ->
      let current_size = def_local ctx ptr_ty in
      let new_space = def_local ctx ptr_ty in
      if' Ptr.((local_get 0) == (const_i32_of_int 0))
        (* nullptr *)
        ~then':(call_ wtf_alloc_fun_name [| I32.local_get 1 |] ptr_ty)
        ~else':(block ~name:"main_logic" [|
          (* current_size <- ptr->total_bytes *)
          local_set current_size (I32.load ~offset:0 (Ptr.local_get 0));

          (* is the last object on the heap *)
          (* if (ptr + size == $free_object) *)
          if' Ptr.(((local_get 0) + (local_get current_size)) == (global_get free_object_var_name))
            ~then':(block [|
              (* $free_object <- ptr + new_size *)
              global_set free_object_var_name Ptr.((local_get 0) + (local_get 1));
              (* ptr->total_bytes <- new_size *)
              I32.store ~offset:0 ~ptr:(Ptr.local_get 0) (I32.local_get 1);
              (* return ptr *)
              break_ "main_logic" ~value:(Ptr.local_get 0);
            |]);

          (* new_space <- wtf_alloc(new_size) *)
          local_set new_space (call_ wtf_alloc_fun_name [| I32.local_get 1 |] ptr_ty);

          (* memcpy(new_space, ptr, current_size) *)
          codegen_memory_copy env ~dest:(Ptr.local_get new_space) ~src:(Ptr.local_get 0) ~size:(I32.local_get current_size);

          (* new_space->total_bytes <- new_size *)
          I32.store ~offset:0 ~ptr:(Ptr.local_get new_space) (I32.local_get 1);

          (* free(ptr) *)
          call_ free_object_fun_name [| Ptr.local_get 0 |] none;

          Ptr.local_get new_space
        |])
    )
    in
    ()
  in

  (* function __wtl_init_object(obj: ptr) *)
  let codegen_wtl_init_object () =
    let _ = def_function init_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun _ ->
        codegen_memory_fill env ~dest:(Ptr.local_get 0) ~value:(const_i32_of_int 0) ~size:(const_i32_of_int 16)
      )
    in
    let _ = export_function init_object_fun_name init_object_fun_name in ()
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

  (* function __wtf_retain_object(obj: ptr) *)
  (* strong counter += 1*)
  let codegen_wtf_retain_object () =
    let _ = def_function retain_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun ctx ->
        let next_count = def_local ctx i32 in
        block [|
          (* next_count <- obj->strong_counter *)
          local_set next_count (I32.load ~offset:8 (Ptr.local_get 0));

          (* next_count <- next_count + 1 *)
          local_set next_count I32.((local_get next_count) + (const_i32_of_int 1));

          (* obj->strong_counter <- next_count *)
          I32.store ~offset:8 ~ptr:(Ptr.local_get 0) (I32.local_get next_count);

        |]
      )
    in
    let _ = export_function retain_object_fun_name retain_object_fun_name in ()
  in

  (* function __wtf_release_object(obj: ptr) *)
  let codegen_wtf_release_object () =
    let _ = def_function release_object_fun_name ~params:[| ptr_ty |] ~ret_ty:none (fun ctx ->
        let next_count = def_local ctx i32 in
        block [|
          (* next_count <- obj->strong_counter *)
          local_set next_count (I32.load ~offset:8 (Ptr.local_get 0));

          (* next_count <- next_count - 1 *)
          local_set next_count I32.((local_get next_count) - (const_i32_of_int 1));

          (* if next_counet != 0 *)
          if' (I32.local_get next_count)
            (* obj->strong_counter <- next_count *)
            ~then':(I32.store ~offset:8 ~ptr:(Ptr.local_get 0) (I32.local_get next_count))
            (* else if (next_count == 0) *)
            (* free_object(ptr) *)
            ~else':(call_ free_object_fun_name [| Ptr.local_get 0 |] none)
          ;
        |]
      )
    in
    let _ = export_function release_object_fun_name release_object_fun_name in ()
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
  codegen_wtf_realloc();
  codegen_wtl_init_object();
  codegen_wtl_free_object();
  codegen_wtf_retain_object();
  codegen_wtf_release_object()
