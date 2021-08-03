open Core_kernel
open Binaryen.Dsl
open Codegen_env
open Waterlang_typing

module M (S: BinaryenModule) = struct
  module Dsl = Binaryen(S)
  open Dsl

  let get_binaryen_ty_by_core_ty env (ty: Core_type.TypeValue.t): Bound.binary_type =
    let open Core_type in
    match ty with
    | Ctor(sym, []) ->
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


  let get_function_params_type env params =
    let open Typedtree.Function in
    let { params_content; _; } = params in
    let types_arr = List.map
      ~f:(fun param -> param.param_ty |> (get_binaryen_ty_by_core_ty env))
      params_content
    in
    type_multiples types_arr

  let unwrap_function_return_type (ty: Core_type.TypeValue.t) =
    let open Core_type in
    match ty with
    | TypeValue.Function f ->
      f.tfun_ret

    | _ -> TypeValue.Unknown

  let set_memory env =
    let strings: bytes list = env.data_segment.allocated_str
      |> Data_segment_allocator.StaticStringPool.to_alist
      |> List.map ~f:(fun (_, value) ->
        let open Data_segment_allocator in
        Buffer.contents_bytes value.data
        )
    in
    let passitive = List.init ~f:(fun _ -> false) (List.length strings) in
    let offsets = List.init
      ~f:(fun _ ->
        let offset = env.config.data_segment_offset in
        const_i32_of_int offset
      )
      (List.length strings)
    in
    let mem_size = env.config.init_mem_size / (64 * 1024) in
    set_memory
      env.module_
      mem_size
      mem_size
      "memory" strings passitive offsets false

  let rec codegen_statements env stat: Bound.expression option =
    let open Typedtree.Statement in
    let { spec; _; } = stat in
    match spec with
    | Function_ fun_ ->
      codegen_function env fun_;
      None
    
    | Expr expr ->
      let expr_result = codegen_expression env expr in
      Some expr_result

    | Return expr_opt ->
      let expr = Option.map
        ~f:(codegen_expression env)
        expr_opt
      in
      let return_expr = return_ expr in
      Some return_expr

    | Binding binding ->
      codegen_binding env binding

    | Semi expr ->
      Some(codegen_expression env expr)

    | While while_ ->
      let name = "while_0" in
      let prev_while_label = env.while_label in
      env.while_label <- Some name;
      let { Typedtree.Statement. while_test; while_block; _; } = while_ in
      let test_expr = binary Dsl.eq_i32 (codegen_expression env while_test) (const_i32_of_int 0) in
      let body = while_block.body in
      let while_block' = block
        (body
        |> List.map ~f:(fun stmt ->
          let expr = codegen_statements env stmt in
          Option.value ~default:(unreachable_exp()) expr
          )

        |> List.rev
        )
      in
      let result = block ~name
        [
          loop "while_0_loop" (block [
            if' test_expr ~then':(Dsl.break_ "while_0");
            while_block';
            break_ "while_0_loop";
          ])
        ]
      in
      env.while_label <- prev_while_label;
      Some result

    | Break _ ->
      (match env.while_label with
      | Some label_name ->
        Some (break_ label_name)

      | _ -> failwith "not in while, can not break"
      )
        


    | _ -> None

  and codegen_binding env binding: Bound.expression option =
    let init_exp = codegen_expression env binding.binding_init in
    let open Core_type.VarSym in
    let local_id =
      match binding.binding_pat.spec with
      | Typedtree.Pattern.Symbol sym -> sym.id_in_scope
    in
    Some(local_set local_id init_exp)

  and codegen_constant env cnst =
    let open Waterlang_parsing.Ast.Literal in
    match cnst with
    | Integer (content, _) ->
      let value = int_of_string content in
      const_i32_of_int value

    | Float (content, _) ->
      let value = float_of_string content in
      const_f64 value

    | String (content, _, _) ->
      Codegen_env.turn_on_string env;
      let value = Data_segment_allocator.add_static_string env.data_segment content in
      let open Data_segment_allocator in
      let str_len = String.length content in
      call_ String_facility.init_string_fun_name_static
        [
          const_i32_of_int value.offset;
          (const_i32_of_int str_len);
        ] i32

    | Char ch ->
      let str = Char.to_string ch in
      let _ = Data_segment_allocator.add_static_string env.data_segment str in
      failwith "not implemented"

    | Boolean true ->
      const_i32_of_int 1

    | Boolean false ->
      const_i32_of_int 0

  and codegen_assign env (left, right) =
    let local_id =
      let open Typedtree.Pattern in
      match left.spec with
      | Symbol sym -> sym.id_in_scope
    in
    let expr = codegen_expression env right in
    local_set local_id expr

  and codegen_expression env expr: Bound.expression =
    let open Typedtree.Expression in
    let { spec; _; } = expr in
    let convert_op raw =
      let open Waterlang_parsing.Asttypes.BinaryOp in
      match raw with
      | Plus -> add_i32
      | Minus -> sub_i32
      | Mult -> mul_i32
      | GreaterThan -> gt_i32
      | LessThan -> lt_i32
      | _ -> failwith "not implemented"
    in
    match spec with
    | Binary(op, left, right) ->
      binary
        (convert_op op)
        (codegen_expression env left)
        (codegen_expression env right)

    | Identifier var_sym ->
      let ty = get_binaryen_ty_by_core_ty env var_sym.def_type in
      local_get var_sym.id_in_scope ty

    | Constant cnst ->
      codegen_constant env cnst

    | Call call ->
      codegen_call env call

    | Assign(left, right) ->
      codegen_assign env (left, right)

    | Block blk ->
      let { Typedtree.Block. body; _; } = blk in
      let exprs =
        List.filter_map
        ~f:(fun stmt -> codegen_statements env stmt)
        body
      in
      Dsl.block exprs

    | If if_expr ->
      let { Typedtree.Expression. if_test; if_consequent; if_alternative; _; } = if_expr in
      let test = codegen_expression env if_test in
      let cons = Option.value ~default:(block []) (codegen_statements env if_consequent) in
      let alt =
        Option.map
        ~f:(fun stmt ->
          let expr_opt = codegen_statements env stmt in
          Option.value ~default:(block []) expr_opt
        )
        if_alternative
      in
      Dsl.if' test ~then':cons ?else':alt

    | _ ->
      unreachable_exp()

  and codegen_call env call =
    let open Typedtree.Expression in
    let open Core_type.VarSym in
    let (callee_sym, _) = call.callee.callee_spec in
    let params =
      call.call_params
      |> List.map ~f:(codegen_expression env)
    in
    match callee_sym.spec with
    | Internal
    | ExternalMethod _ ->
      let get_function_name_by_callee callee =
        match callee.callee_spec with
        | (callee_sym, []) -> callee_sym.name
        | (callee_sym, _arr) ->
          callee_sym.name
      in
      let callee_name: string = get_function_name_by_callee call.callee in
      let callee_ty = call.callee.callee_ty in
      let return_ty =
        callee_ty
        |> unwrap_function_return_type
        |> (get_binaryen_ty_by_core_ty env)
      in
      call_ callee_name params return_ty

    | _ ->
      failwith "unreachable"

  and codegen_function env function_ =
    let open Typedtree.Function in
    let params_ty = get_function_params_type env function_.header.params in
    let vars_ty =
      function_.assoc_scope.var_symbols
      |> Scope.SymbolTable.to_alist
      |> List.map
          ~f:(fun (_, var_sym) -> Core_type.VarSym.(get_binaryen_ty_by_core_ty env var_sym.def_type))

    in
    let { body; _; } = function_ in
    let block_contents =
      match body with
      | Fun_block_body block ->
        begin
          let open Typedtree.Block in
          let { body; _; } = block in
          body
          |> List.filter_map ~f:(codegen_statements env)
        end

      | Fun_expression_body expr ->
        [ codegen_expression env expr ]

    in

    let finalizers: Bound.expression list =
      function_.assoc_scope.var_symbols
      |> Scope.SymbolTable.to_alist
      |> List.filter_map
        ~f:(fun (_, (sym: Core_type.VarSym.t)) ->
          let open Core_type in
          let open VarSym in
          let open TypeValue in
          match sym.def_type with
          | Ctor(type_sym, []) ->
            (match type_sym.spec with
            | TypeSym.Primitive -> None
            | TypeSym.Object ->
              let exp =
                call_ Allocator_facility.release_object_fun_name [ Ptr.local_get sym.id_in_scope ] none
              in
              Some exp

            | _ -> failwith "not implemented 1")
          | _ ->
            let def_type_msg =
              Format.asprintf "can not generate finalize for %s:%a" sym.name TypeValue.pp sym.def_type
            in
            failwith def_type_msg
        )
    in
    let exp = block (List.concat [block_contents; finalizers]) in

    let id = function_.header.id in
    let id_name = id.name in
    let ret_ty =
      id.def_type
      |> unwrap_function_return_type
      |> get_binaryen_ty_by_core_ty env
    in
    let _fun = Dsl.function_ ~name:id_name ~params_ty ~ret_ty ~vars_ty ~content:exp in
    let _ = export_function id_name id_name in
    ()

  and codgen_external_method (env: Codegen_env.t) =
    let scope = env.program.root_scope in
    Scope.SymbolTable.to_alist scope.var_symbols
    |> List.iter
      ~f:(fun (key, value) ->
        let open Core_type.VarSym in
        let def_type = value.def_type in
        match def_type with
        | Core_type.TypeValue.Function fun_type ->
          begin
            let params_ty = 
              fun_type.tfun_params
              |> List.map ~f:(fun (_, t) -> get_binaryen_ty_by_core_ty env t)
              |> type_multiples
            in
            let ret_ty = get_binaryen_ty_by_core_ty env fun_type.tfun_ret in
            match value.spec with
            | ExternalMethod(extern_name, extern_base_name) ->
              Dsl.import_function ~intern_name:key ~extern_name ~extern_base_name ~params_ty ~ret_ty
            | _ -> ()

          end
        | _ -> ()

      );

  and codegen_program (env: Codegen_env.t) =
    Bound.set_debug_info (not env.config.release);
    let { Program. tree; _; } = env.program in
    let { Typedtree. tprogram_statements } = tree in
    let _ = List.map ~f:(codegen_statements env) tprogram_statements in

    if Codegen_env.needs_allocator env then (
      Allocator_facility.codegen_allocator_facility env;
    );

    if Codegen_env.needs_string env then (
      String_facility.codegen_string_facility env;
    );

    codgen_external_method env;

    set_memory env
  
end

let codegen program config : string =
  let env = Codegen_env.create config program in
  let module Cg = M(struct
      let m = env.module_
      let ptr_ty = Codegen_env.ptr_ty env
    end)
  in
  Cg.codegen_program env;
  let str = Bound.emit_text env.module_ in
  str

let codegen_binary env path : unit =
  let module Cg = M(struct
      let m = env.module_
      let ptr_ty = Codegen_env.ptr_ty env
    end)
  in
  Cg.codegen_program env;
  emit_binary env.module_ path;
  let js_glue_content = Js_glue.dump_js_glue env in
  Out_channel.write_all (path ^ ".js") ~data:js_glue_content
