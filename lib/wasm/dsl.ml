
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

  let const_wrap maker value =
    let lit = maker value in
    C_bindings.make_exp_const M.m lit

  let const_i32 = const_wrap C_bindings.make_literal_i32

  let const_i64 = const_wrap C_bindings.make_literal_i64

  let const_f32 = const_wrap C_bindings.make_literal_f32

  let const_f64 = const_wrap C_bindings.make_literal_f64

  let local_get index ty =
    C_bindings.make_exp_local_get M.m index ty

  let local_set index exp =
    C_bindings.make_exp_local_set M.m index exp

  let store ~bytes ~offset ~align ~ptr ~value ~ty =
    C_bindings.make_exp_store M.m bytes offset align ptr value ty

  let unreachable_exp () =
    C_bindings.make_exp_unrechable M.m

  let return_ exp =
    C_bindings.make_exp_return M.m exp

  let binary op left right =
    C_bindings.make_exp_binary M.m op left right

  let call_ name params ty =
    C_bindings.make_exp_call M.m name params ty

  let memory_fill ~dest ~value ~size =
    C_bindings.make_exp_memory_fill M.m dest value size

  let function_ ~name ~params_ty ~ret_ty ~vars_ty ~content =
    C_bindings.add_function M.m name params_ty ret_ty vars_ty content

  let export_function intern_name export_name =
    C_bindings.add_function_export M.m intern_name export_name
  
end
