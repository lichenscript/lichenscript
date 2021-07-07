
module type BinaryenModule = sig

  val m: C_bindings.m

end

module Binaryen (M: BinaryenModule) = struct
  let none = C_bindings.make_ty_none()

  let i32 = C_bindings.make_ty_int32()

  let i64 = C_bindings.make_ty_int64 ()

  let f32 = C_bindings.make_ty_f32 ()

  let f64 = C_bindings.make_ty_f64 ()

  let add_i32 = C_bindings.make_op_add_i32 ()

  let sub_i32 = C_bindings.make_op_sub_i32 ()

  let mul_i32 = C_bindings.make_op_sub_i32 ()

  let any_ref = C_bindings.make_ty_any_ref ()

  let unreachable = C_bindings.make_ty_unreachable ()

  let auto = C_bindings.make_ty_auto ()

  let const_wrap maker value =
    let lit = maker value in
    C_bindings.make_exp_const M.m lit

  let block ?name children ty =
    C_bindings.make_exp_block M.m name children ty

  let const_i32 = const_wrap C_bindings.make_literal_i32

  let const_i32_of_int value =
    const_i32 (Int32.of_int value)

  let const_i64 = const_wrap C_bindings.make_literal_i64

  let const_f32 = const_wrap C_bindings.make_literal_f32

  let const_f64 = const_wrap C_bindings.make_literal_f64

  let local_get = C_bindings.make_exp_local_get M.m

  let local_set = C_bindings.make_exp_local_set M.m

  let global_set = C_bindings.make_exp_global_set M.m

  let global_get = C_bindings.make_exp_global_get M.m

  let store ~bytes ~offset ~align ~ptr ~value ~ty =
    C_bindings.make_exp_store M.m bytes offset align ptr value ty

  let unreachable_exp () =
    C_bindings.make_exp_unrechable M.m

  let return_ = C_bindings.make_exp_return M.m

  let if_ = C_bindings.make_exp_if M.m

  let binary = C_bindings.make_exp_binary M.m

  let call_ = C_bindings.make_exp_call M.m

  let memory_fill ~dest ~value ~size =
    C_bindings.make_exp_memory_fill M.m dest value size

  let memory_copy ~dest ~src ~size =
    C_bindings.make_exp_memory_copy M.m dest src size

  let function_ ~name ~params_ty ~ret_ty ~vars_ty ~content =
    C_bindings.add_function M.m name params_ty ret_ty vars_ty content

  let export_function intern_name export_name =
    C_bindings.add_function_export M.m intern_name export_name

  let import_function ~intern_name ~extern_name ~extern_base_name ~params_ty ~ret_ty =
    C_bindings.add_function_import M.m intern_name extern_name extern_base_name params_ty ret_ty

  let add_global_var ~name ty ~mut ~init =
    C_bindings.add_global M.m name ty mut init
  
end
