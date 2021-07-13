open Core_kernel
open Binaryen
open Codegen_env
open Waterlang_typing

module M (S: Dsl.BinaryenModule) = struct
  module Dsl = Dsl.Binaryen(S)
  open Dsl

  let get_binaryen_ty_by_core_ty env (ty: Core_type.TypeValue.t): C_bindings.ty =
    let open Core_type in
    match ty with
    | Ctor sym ->
      if (TypeSym.builtin sym) then (
        match (TypeSym.name sym) with
        | "i32" -> i32
        | "i64" -> i64
        | "f32" -> f32
        | "f64" -> f64
        | "char" -> i32
        | "boolean" -> i32
        | "string" -> (Codegen_env.ptr_ty env)
        | _ -> unreachable
      ) else
        failwith "not builtin"

    | Unit -> none
    | _ -> unreachable

  let unwrap_function_return_type (ty: Core_type.TypeValue.t) =
    let open Core_type in
    match ty with
    | TypeValue.Function f ->
      f.tfun_ret

    | _ -> TypeValue.Unknown

  let set_memory env =
    let strings: bytes array = env.data_segment.allocated_str
      |> Data_segment_allocator.StaticStringPool.to_alist
      |> List.map ~f:(fun (_, value) ->
        let open Data_segment_allocator in
        Buffer.contents_bytes value.data
        )
      |> List.to_array
    in
    let passitive = Array.init ~f:(fun _ -> false) (Array.length strings) in
    let offsets = Array.init
      ~f:(fun _ ->
        let offset = env.config.data_segment_offset in
        const_i32_of_int offset
      )
      (Array.length strings)
    in
    let mem_size = env.config.init_mem_size / (64 * 1024) in
    C_bindings.set_memory
      env.module_
      mem_size
      mem_size
      "memory" strings passitive offsets false

  let rec codegen_statements env stat: C_bindings.exp option =
    let open Typedtree in
    let { tstmt_desc; _; } = stat in
    match tstmt_desc with
    | Tstmt_function fun_ ->
      codegen_function env fun_;
      None
    
    | Tstmt_expr expr ->
      let expr_result = codegen_expression env expr in
      Some expr_result

    | Tstmt_return expr_opt ->
      let expr = Option.map
        ~f:(codegen_expression env)
        expr_opt
      in
      let return_expr = return_ expr in
      Some return_expr

    | Tstmt_binding binding ->
      codegen_binding env binding

    | Tstmt_semi expr ->
      Some(codegen_expression env expr)

    | _ -> None

  and codegen_binding env binding: C_bindings.exp option =
    let init_exp = codegen_expression env binding.tbinding_init in
    let open Typedtree in
    let open Core_type.VarSym in
    let local_id =
      match binding.tbinding_pat.tpat_desc with
      | Tpat_symbol sym -> sym.id_in_scope
    in
    Some(local_set local_id init_exp)

  and codegen_constant env cnst =
    let open Waterlang_parsing.Ast in
    match cnst with
    | Pconst_integer (content, _) ->
      let value = int_of_string content in
      const_i32_of_int value

    | Pconst_float (content, _) ->
      let value = float_of_string content in
      const_f64 value

    | Pconst_string (content, _, _) ->
      Codegen_env.turn_on_string env;
      let value = Data_segment_allocator.add_static_string env.data_segment content in
      let open Data_segment_allocator in
      let str_len = String.length content in
      call_ String_facility.init_string_fun_name_static
        [|
          const_i32_of_int value.offset;
          (const_i32_of_int str_len);
        |] i32

    | Pconst_char ch ->
      let str = Char.to_string ch in
      let _ = Data_segment_allocator.add_static_string env.data_segment str in
      failwith "not implemented"

    | Pconst_boolean true ->
      const_i32_of_int 1

    | Pconst_boolean false ->
      const_i32_of_int 0

  and codegen_expression env expr: C_bindings.exp =
    let open Typedtree in
    let { texp_desc; _; } = expr in
    let convert_op raw =
      let open Waterlang_parsing.Asttypes.BinaryOp in
      match raw with
      | Plus -> add_i32
      | Minus -> sub_i32
      | Mult -> mul_i32
      | _ -> failwith "not implemented"
    in
    match texp_desc with
    | Texp_binary(op, left, right) ->
      binary
        (convert_op op)
        (codegen_expression env left)
        (codegen_expression env right)

    | Texp_identifier var_sym ->
      let ty = get_binaryen_ty_by_core_ty env var_sym.def_type in
      local_get var_sym.id_in_scope ty

    | Texp_constant cnst ->
      codegen_constant env cnst

    | Texp_call call ->
      codegen_call env call

    | _ ->
      unreachable_exp()

  and codegen_call env call =
    let open Typedtree in
    let open Core_type.VarSym in
    let (callee_sym, prop_names) = call.tcallee.tcallee_spec in
    let params =
      call.tcall_params
      |> List.map ~f:(codegen_expression env)
      |> List.to_array
    in
    match callee_sym.spec with
    | Internal ->
      let get_function_name_by_callee callee =
        match callee.tcallee_spec with
        | (callee_sym, []) -> callee_sym.name
        | (callee_sym, _arr) ->
          callee_sym.name
      in
      let callee_name: string = get_function_name_by_callee call.tcallee in
      let callee_ty = call.tcallee.tcallee_ty in
      let return_ty =
        callee_ty
        |> unwrap_function_return_type
        |> (get_binaryen_ty_by_core_ty env)
      in
      call_ callee_name params return_ty

    | ExternalModule _mod ->
      let call_var_sym: Core_type.VarSym.t =
        List.fold
        ~init:callee_sym
        ~f:(fun sym name_spec ->
          match sym.spec with
          | Internal -> failwith "unreachable"
          | ExternalModule mod_ ->
            let child_name = match name_spec with
              | `Property name -> name
              | _ -> failwith "not implemented"
            in
            let next_sym = Core_type.PropsMap.find_exn mod_ child_name in
            next_sym

          | ExternalMethod _ -> failwith "not implemented")
        prop_names
      in
      let return_ty =
        call_var_sym.def_type
        |> unwrap_function_return_type
        |> (get_binaryen_ty_by_core_ty env)
      in
      let name =
        match call_var_sym.spec with
        | ExternalMethod m -> m
        | _ -> failwith "unreachable"
      in
      call_ name params return_ty

    | ExternalMethod _ ->
      failwith "not implemented"

  and codegen_function env function_ =
    let parms_type params =
      let open Typedtree in
      let { tparams_content; _; } = params in
      let params_arr = List.to_array tparams_content in
      let types_arr = Array.map
        ~f:(fun param -> param.tparam_ty |> (get_binaryen_ty_by_core_ty env))
        params_arr
      in
      C_bindings.make_ty_multiples types_arr
    in

    let params_ty = parms_type function_.tfun_params in
    let vars_ty =
      function_.tfun_assoc_scope.var_symbols
      |> Scope.SymbolTable.to_alist
      |> List.map
          ~f:(fun (_, var_sym) -> Core_type.VarSym.(get_binaryen_ty_by_core_ty env var_sym.def_type))
      |> List.to_array

    in
    let { Typedtree. tfun_body; _; } = function_ in
    let exp =
      match tfun_body with
      | Typedtree.Tfun_block_body block ->
        begin
          let { Typedtree. tblk_body; _; } = block in
          let expressions = 
            tblk_body
            |> List.filter_map ~f:(codegen_statements env)
          in
          Dsl.block (List.to_array expressions)
        end

      | Typedtree.Tfun_expression_body expr ->
        codegen_expression env expr
    in

    let id = function_.tfun_id in
    let id_name = id.name in
    let ret_ty =
      function_.tfun_id.def_type
      |> unwrap_function_return_type
      |> get_binaryen_ty_by_core_ty env
    in
    let _fun = Dsl.function_ ~name:id_name ~params_ty ~ret_ty ~vars_ty ~content:exp in
    let _ = export_function id_name id_name in
    ()

  and codegen_program env (program: Program.t) =
    let { Program. tree } = program in
    let { Typedtree. tprogram_statements } = tree in
    let _ = List.map ~f:(codegen_statements env) tprogram_statements in

    if Codegen_env.needs_allocator env then (
      Allocator_facility.codegen_allocator_facility env;
    );

    if Codegen_env.needs_string env then (
      String_facility.codegen_string_facility env;
    );

    set_memory env
  
end

let codegen program config : string =
  let env = Codegen_env.create config in
  let module Cg = M(struct
      let m = env.module_
      let ptr_ty = Codegen_env.ptr_ty env
    end)
  in
  Cg.codegen_program env program;
  let str = C_bindings.module_emit_text env.module_ in
  str

let codegen_binary program env path : unit =
  let module Cg = M(struct
      let m = env.module_
      let ptr_ty = Codegen_env.ptr_ty env
    end)
  in
  Cg.codegen_program env program;
  C_bindings.module_emit_binary_to_file env.module_ path;
  let js_glue_content = Js_glue.dump_js_glue env in
  Out_channel.write_all (path ^ ".js") ~data:js_glue_content
