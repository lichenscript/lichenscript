open Binaryen

let init_array_fun_name = "__wtl_init_i32_array"

let array_length_fun_name = "__wtf_i32_array_length"

let array_push_fun_name = "__wtf_i32_array_push"

let codegen_array_facility (env: Codegen_env.t) =
  let module Dsl =
    Dsl.Binaryen(struct
      let ptr_ty = Codegen_env.ptr_ty env
      let m = env.module_
    end)
  in
  let open Dsl in

  (* function __wtf_init_i32_array(capacity: i32) *)
  let codegen_init_i32_array () =
    let _ = def_function init_array_fun_name ~params:[| i32 |] ~ret_ty:ptr_ty (fun _ ->
      unreachable_exp()
    )
    in
    let _ = export_function init_array_fun_name init_array_fun_name in ()
  in

  let codegen_i32_array_length () =
    let _ = def_function array_length_fun_name ~params:[| ptr_ty |] ~ret_ty:i32 (fun _ ->
      I32.load ~offset:16 (Ptr.local_get 0)
    )
    in
    let _ = export_function array_length_fun_name array_length_fun_name in ()
  in

  (* function __wtf_i32_array_push(arr: ptr, elment: i32) *)
  let codegen_i32_array_push () =
    let _ = def_function array_push_fun_name ~params:[| i32; i32 |] ~ret_ty:i32 (fun ctx ->
      let current_length = def_local ctx i32 in
      let data_ptr = def_local ctx ptr_ty in
      let current_cap = def_local ctx i32 in
      let data_offset = def_local ctx i32 in
      block [|
        local_set current_length (I32.load ~offset:16 (Ptr.local_get 0));
        local_set data_ptr (Ptr.load ~offset:20 (Ptr.local_get 0));
        local_set current_cap (Ptr.load ~offset:8 (Ptr.local_get data_ptr));

        (* if cap is full *)
        if_ I32.(((local_get current_length) + (const_i32_of_int 1)) >= (local_get current_cap))
        (unreachable_exp ())
        (Some (block [|
          (* data_offset = sizeof(i32) * current_length *)
          I32.(local_set data_offset ((local_get current_length) * (const_i32_of_int 4)));

          (* (data_ptr + data_offset)[12] = data *)
          I32.(store ~offset:12 ~ptr:((local_get data_ptr) + (local_get data_offset)) (local_get 1));

          (* ptr->length += 1 *)
          I32.(store ~offset:16 ~ptr:(Ptr.local_get 0) ((local_get current_length) + (const_i32_of_int 1)));

        |]));

      |]
    )
    in
    let _ = export_function array_push_fun_name array_push_fun_name in ()
  in

  codegen_init_i32_array ();
  codegen_i32_array_length ();
  codegen_i32_array_push ()
