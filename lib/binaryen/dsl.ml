open Core_kernel

module type BinaryenModule = sig

  val ptr_ty: C_bindings.ty

  val m: C_bindings.m

end

module type BinaryenTypeContainer = sig

  val size: int

  val ty: C_bindings.ty

  val add_op: C_bindings.op

  val sub_op: C_bindings.op

  val mul_op: C_bindings.op

  val div_op: C_bindings.op

  val gt_op: C_bindings.op

  val ge_op: C_bindings.op

  val lt_op: C_bindings.op

  val le_op: C_bindings.op

  val eq_op: C_bindings.op

end

module VarOperator (M: BinaryenModule) (T: BinaryenTypeContainer) = struct

  let size = T.size

  let local_get index = C_bindings.make_exp_local_get M.m index T.ty

  let global_get name =
    C_bindings.make_exp_global_get M.m name T.ty

  let store ~offset ?(align = 0) ~ptr content =
    C_bindings.make_exp_store
      M.m T.size offset align ptr content T.ty

  let load ?(signed = true) ~offset ?(align = 0) ptr =
    C_bindings.make_exp_load
      M.m T.size signed offset align T.ty ptr

  let add left right =
    C_bindings.make_exp_binary
      M.m T.add_op left right

  let (+) = add

  let sub left right =
    C_bindings.make_exp_binary
      M.m T.sub_op left right

  let (-) = sub

  let mul left right =
    C_bindings.make_exp_binary
      M.m T.mul_op left right

  let ( * ) = mul

  let div left right =
    C_bindings.make_exp_binary
      M.m T.div_op left right

  let (/) = div

  let (>) left right =
    C_bindings.make_exp_binary
      M.m T.gt_op left right

  let (<) left right =
    C_bindings.make_exp_binary
      M.m T.lt_op left right

  let (>=) left right =
    C_bindings.make_exp_binary
      M.m T.ge_op left right

  let (<=) left right =
    C_bindings.make_exp_binary
      M.m T.le_op left right

  let (==) left right =
    C_bindings.make_exp_binary
      M.m T.eq_op left right
  
end

module Binaryen (M: BinaryenModule) = struct

  let none = C_bindings.make_ty_none()

  let i32 = C_bindings.make_ty_int32()

  let i64 = C_bindings.make_ty_int64 ()

  let f32 = C_bindings.make_ty_f32 ()

  let f64 = C_bindings.make_ty_f64 ()

  let ptr_ty = M.ptr_ty

  let add_i32 = C_bindings.make_op_add_i32 ()

  let sub_i32 = C_bindings.make_op_sub_i32 ()

  let mul_i32 = C_bindings.make_op_mul_i32 ()

  let div_i32 = C_bindings.make_op_div_i32 ()

  let lt_i32 = C_bindings.make_op_lt_i32 ()

  let gt_i32 = C_bindings.make_op_gt_i32 ()

  let eq_i32 = C_bindings.make_op_eq_i32 ()

  let ne_i32 = C_bindings.make_op_ne_i32 ()

  let any_ref = C_bindings.make_ty_any_ref ()

  let unreachable = C_bindings.make_ty_unreachable ()

  let auto = C_bindings.make_ty_auto ()

  let const_wrap maker value =
    let lit = maker value in
    C_bindings.make_exp_const M.m lit

  let block ?name ?(ty = none) children  =
    C_bindings.make_exp_block M.m name children ty

  let const_i32 = const_wrap C_bindings.make_literal_i32

  let const_i32_of_int value =
    const_i32 (Int32.of_int_exn value)

  let const_i64 = const_wrap C_bindings.make_literal_i64

  let const_f32 = const_wrap C_bindings.make_literal_f32

  let const_f64 = const_wrap C_bindings.make_literal_f64

  let local_get = C_bindings.make_exp_local_get M.m

  let local_set = C_bindings.make_exp_local_set M.m

  let global_set = C_bindings.make_exp_global_set M.m

  let global_get = C_bindings.make_exp_global_get M.m

  let store ~bytes ~offset ~align ~ptr ~value ~ty =
    C_bindings.make_exp_store M.m bytes offset align ptr value ty

  let load ~bytes ~signed ~offset ~align ~ty ptr =
    C_bindings.make_exp_load M.m bytes signed offset align ty ptr

  let unreachable_exp () =
    C_bindings.make_exp_unrechable M.m

  let return_ = C_bindings.make_exp_return M.m

  let if' ?else' test ~then' = C_bindings.make_exp_if M.m test then' else'

  let loop = C_bindings.make_exp_loop M.m

  let break_ ?cond ?value name = C_bindings.make_exp_break M.m name cond value

  let binary = C_bindings.make_exp_binary M.m

  let call_ = C_bindings.make_exp_call M.m

  let memory_fill ~dest ~value ~size =
    C_bindings.make_exp_memory_fill M.m dest value size

  let memory_copy ~dest ~src ~size =
    C_bindings.make_exp_memory_copy M.m dest src size

  let function_ ~name ~params_ty ~ret_ty ~vars_ty ~content =
    C_bindings.add_function M.m name params_ty ret_ty vars_ty content

  type function_local_var_allocator = {
    mutable value: int;
    mutable def_ty: C_bindings.ty list;
  }

  let def_local allocator ty =
    let tmp = allocator.value in
    allocator.value <- tmp + 1;
    allocator.def_ty <- ty::allocator.def_ty;
    tmp

  let def_function name ~params ~ret_ty callback =
    let params_ty = C_bindings.make_ty_multiples params in
    let allocator = {
      value = Array.length params;
      def_ty = [];
    } in
    let exp = callback allocator in
    function_ ~name ~params_ty ~ret_ty
      ~vars_ty:(allocator.def_ty |> List.rev |> List.to_array) ~content:exp

  let export_function intern_name export_name =
    C_bindings.add_function_export M.m intern_name export_name

  let import_function ~intern_name ~extern_name ~extern_base_name ~params_ty ~ret_ty =
    C_bindings.add_function_import M.m intern_name extern_name extern_base_name params_ty ret_ty

  let add_global_var ~name ty ~mut ~init =
    C_bindings.add_global M.m name ty mut init

  module I32 = VarOperator(M)(struct
    let size = 4
    let ty = i32

    let add_op = C_bindings.make_op_add_i32()

    let sub_op = C_bindings.make_op_sub_i32()

    let mul_op = C_bindings.make_op_mul_i32()

    let div_op = C_bindings.make_op_div_i32()

    let gt_op = C_bindings.make_op_gt_i32()

    let lt_op = C_bindings.make_op_lt_i32()

    let ge_op = C_bindings.make_op_ge_i32()

    let le_op = C_bindings.make_op_le_i32()

    let eq_op = C_bindings.make_op_eq_i32()

  end)

  module Ptr = VarOperator(M)(struct
    let size = 4
    let ty = ptr_ty

    let add_op = C_bindings.make_op_add_i32()

    let sub_op = C_bindings.make_op_sub_i32()

    let mul_op = C_bindings.make_op_mul_i32()

    let div_op = C_bindings.make_op_div_i32()

    let gt_op = C_bindings.make_op_gt_i32()

    let lt_op = C_bindings.make_op_lt_i32()

    let ge_op = C_bindings.make_op_ge_i32()

    let le_op = C_bindings.make_op_le_i32()

    let eq_op = C_bindings.make_op_eq_i32()

  end)
  
end
