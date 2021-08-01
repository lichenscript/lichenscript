
open Core_kernel
open Bindings

module Bound = Bindings(B)

let module_create = Bound.module_create

let none = Bound.type_none()

let i32 = Bound.type_int32()

let i64 = Bound.type_int64 ()

let f32 = Bound.type_f32 ()

let f64 = Bound.type_f64 ()

let type_multiples params =
  let params_arr = Ctypes.CArray.of_list Bound.binary_type params in
  let params_len = Unsigned.UInt32.of_int (List.length params) in
  Bound.type_multiples (Ctypes.CArray.start params_arr) params_len

let emit_binary m path =
  let result = Bound.emit_binary m "" in
  let bytes = Ctypes.getf result Bound.binary_result_binary in
  let raw_size = Ctypes.getf result Bound.binary_result_size in
  Bound.dump_bytes_to_path bytes raw_size path;
  Bound.clean_binary_result result

module type BinaryenModule = sig

  val ptr_ty: Bound.binary_type

  val m: Bound.module_

end

module type BinaryenTypeContainer = sig

  val size: int

  val ty: Bound.binary_type

  val add_op: Bound.binary_op

  val sub_op: Bound.binary_op

  val mul_op: Bound.binary_op

  val div_op: Bound.binary_op

  val gt_op: Bound.binary_op

  val ge_op: Bound.binary_op

  val lt_op: Bound.binary_op

  val le_op: Bound.binary_op

  val eq_op: Bound.binary_op

end

module VarOperator (M: BinaryenModule) (T: BinaryenTypeContainer) = struct

  let size = T.size

  let local_get index = Bound.expr_local_get M.m index T.ty

  let global_get name = Bound.expr_global_get M.m name T.ty

  let store ~offset ?(align = 0) ~ptr content =
    Bound.expr_store M.m T.size offset align ptr content T.ty

  let load ?(signed = true) ~offset ?(align = 0) ptr =
    Bound.expr_load M.m T.size signed offset align T.ty ptr

  let add left right =
    Bound.expr_binary M.m T.add_op left right

  let (+) = add

  let sub left right =
    Bound.expr_binary M.m T.sub_op left right

  let (-) = sub

  let mul left right =
    Bound.expr_binary M.m T.mul_op left right

  let ( * ) = mul

  let div left right =
    Bound.expr_binary M.m T.div_op left right

  let (/) = div

  let (>) left right =
    Bound.expr_binary M.m T.gt_op left right

  let (<) left right =
    Bound.expr_binary M.m T.lt_op left right

  let (>=) left right =
    Bound.expr_binary M.m T.ge_op left right

  let (<=) left right =
    Bound.expr_binary M.m T.le_op left right

  let (==) left right =
    Bound.expr_binary M.m T.eq_op left right
  
end

module Binaryen (M: BinaryenModule) = struct

  let ptr_ty = M.ptr_ty

  let add_i32 = Bound.add_i32()

  let sub_i32 = Bound.sub_i32()

  let mul_i32 = Bound.mul_i32()

  let div_i32 = Bound.div_si32()

  let lt_i32 = Bound.lt_si32()

  let gt_i32 = Bound.gt_si32()

  let eq_i32 = Bound.eq_si32()

  let ne_i32 = Bound.ne_i32()

  let any_ref = Bound.type_any_ref()

  let unreachable = Bound.type_unreachable()

  let auto = Bound.type_auto()

  let const_wrap maker value =
    let lit = maker value in
    Bound.expr_const M.m lit

  let block ?name ?(ty = none) children  =
    let arr = Ctypes.CArray.of_list Bound.expression children in
    let arr_len = Unsigned.Size_t.of_int (Ctypes.CArray.length arr) in
    let name =
      match name with
      | Some name ->
        let arr = Ctypes.CArray.of_string name in
        Ctypes.CArray.start arr

      | None -> Ctypes.(coerce (ptr void) (ptr char) null)
    in
    Bound.expr_block M.m name (Ctypes.CArray.start arr) arr_len ty

  let const_i32 = const_wrap Bound.literal_int32

  let const_i32_of_int value =
    const_i32 (Int32.of_int_exn value)

  let const_i64 = const_wrap Bound.literal_int64

  let const_f32 = const_wrap Bound.literal_f32

  let const_f64 = const_wrap Bound.literal_f64

  let local_get = Bound.expr_local_get M.m

  let local_set = Bound.expr_local_set M.m

  let global_set = Bound.expr_global_set M.m

  let global_get = Bound.expr_global_get M.m

  let store ~bytes ~offset ~align ~ptr ~value ~ty =
    Bound.expr_store M.m bytes offset align ptr value ty

  let load ~bytes ~signed ~offset ~align ~ty ptr =
    Bound.expr_load M.m bytes signed offset align ty ptr

  let unreachable_exp () =
    Bound.expr_unreachable M.m

  let return_ expr =
    let expr = Option.value ~default:(Ctypes.(coerce (ptr void) (ptr void) null)) expr in
    Bound.expr_return M.m expr

  let if' ?else' test ~then' =
    let else' = Option.value ~default:(Ctypes.(coerce (ptr void) (ptr void) null)) else' in
    Bound.expr_if M.m test then' else'

  let loop = Bound.expr_loop M.m

  let break_ ?cond ?value name =
    let cond = Option.value ~default:(Ctypes.(coerce (ptr void) (ptr void) null)) cond in
    let value = Option.value ~default:(Ctypes.(coerce (ptr void) (ptr void) null)) value in
    Bound.expr_break M.m name cond value

  let binary = Bound.expr_binary M.m

  let call_ name params =
    let params_arr = Ctypes.CArray.of_list Bound.expression params in
    let params_len = Unsigned.Size_t.of_int (List.length params) in
    Bound.expr_call M.m name (Ctypes.CArray.start params_arr) params_len

  let memory_fill ~dest ~value ~size =
    Bound.expr_memory_fill M.m dest value size

  let memory_copy ~dest ~src ~size =
    Bound.expr_memory_copy M.m dest src size

  let function_ ~name ~params_ty ~ret_ty ~vars_ty ~content : Bound.function_ =
    let vars_ty_arr = Ctypes.CArray.of_list Bound.binary_type vars_ty in
    let vars_ty_len = Unsigned.Size_t.of_int (List.length vars_ty) in
    Bound.add_function M.m name params_ty ret_ty (Ctypes.CArray.start vars_ty_arr) vars_ty_len content

  type function_local_var_allocator = {
    mutable value: int;
    mutable def_ty: Bound.binary_type list;
  }

  let def_local allocator ty =
    let tmp = allocator.value in
    allocator.value <- tmp + 1;
    allocator.def_ty <- ty::allocator.def_ty;
    tmp

  let def_function name ~params ~ret_ty callback =
    let params_ty = type_multiples params in
    let allocator = {
      value = List.length params;
      def_ty = [];
    } in
    let exp = callback allocator in
    function_ ~name ~params_ty ~ret_ty
      ~vars_ty:(allocator.def_ty |> List.rev) ~content:exp

  let export_function intern_name export_name =
    Bound.add_function_export M.m intern_name export_name

  let import_function ~intern_name ~extern_name ~extern_base_name ~params_ty ~ret_ty =
    Bound.add_function_import M.m intern_name extern_name extern_base_name params_ty ret_ty

  let add_global_var ~name ty ~mut ~init =
    Bound.add_global M.m name ty mut init

  let set_memory module_ size1 size2 export_name segments passitive offsets b =
    let segments = List.map ~f:(Bytes.to_string) segments in
    let segments_arr = Ctypes.CArray.of_list Ctypes.string segments in
    let segments_len = Unsigned.Size_t.of_int (List.length segments) in
    let segments_size =
      List.map
      ~f:(fun str ->
        let len = String.length str in
        Unsigned.UInt32.of_int len
      )
      segments
    in
    let segments_size_arr = Ctypes.CArray.of_list Ctypes.uint32_t segments_size in
    let passitive_arr = Ctypes.CArray.of_list Ctypes.bool passitive in
    if (List.length segments) <> (List.length passitive) then 
      failwith "segments != passtives"
    ;

    let offsets_arr = Ctypes.CArray.of_list Bound.expression offsets in

    Bound.set_memory
      module_ size1 size2 export_name 
      (Ctypes.CArray.start segments_arr) (Ctypes.CArray.start passitive_arr)
      (Ctypes.CArray.start offsets_arr) (Ctypes.CArray.start segments_size_arr) segments_len b

  module I32 = VarOperator(M)(struct
    let size = 4
    let ty = i32

    let add_op = Bound.add_i32()

    let sub_op = Bound.sub_i32()

    let mul_op = Bound.mul_i32()

    let div_op = Bound.div_si32()

    let gt_op = Bound.gt_si32()

    let lt_op = Bound.lt_si32()

    let ge_op = Bound.ge_si32()

    let le_op = Bound.le_si32()

    let eq_op = Bound.eq_si32()

  end)

  module Ptr = VarOperator(M)(struct
    let size = 4
    let ty = ptr_ty

    let add_op = Bound.add_i32()

    let sub_op = Bound.sub_i32()

    let mul_op = Bound.mul_i32()

    let div_op = Bound.div_si32()

    let gt_op = Bound.gt_si32()

    let lt_op = Bound.lt_si32()

    let ge_op = Bound.ge_si32()

    let le_op = Bound.le_si32()

    let eq_op = Bound.eq_si32()

  end)
end
