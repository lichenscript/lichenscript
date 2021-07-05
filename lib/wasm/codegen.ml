open Core_kernel
open Codegen_env
open Waterlang_typing

module M (S: Dsl.BinaryenModule) = struct
  module Dsl = Dsl.Binaryen(S)
  open Dsl

  let rec codegen_statements env stat =
    let open Typedtree in
    let { tstmt_desc; _; } = stat in
    match tstmt_desc with
    | Tstmt_function fun_ ->
      env.last_expr <- None;
      codegen_function env fun_
    
    | Tstmt_expr expr ->
      let expr_result = codegen_expression env expr in
      env.last_expr <- (Some expr_result)

    | Tstmt_return expr_opt ->
      let expr = Option.map
        ~f:(codegen_expression env)
        expr_opt
      in
      let return_expr = return_ expr in
      env.last_expr <- (Some return_expr)

    | _ ->
      ()

  and codegen_constant env cnst =
    let open Waterlang_parsing.Ast in
    match cnst with
    | Pconst_integer (content, _) ->
      let value = int_of_string content in
      const_i32 (Int32.of_int_exn value)

    | Pconst_float (content, _) ->
      let value = float_of_string content in
      const_f64 value

    | Pconst_string (content, _, _) ->
      let _ = Codegen_env.add_static_string env content in
      failwith "not implemented"

    | Pconst_char ch ->
      let str = Char.to_string ch in
      let _ = Codegen_env.add_static_string env str in
      failwith "not implemented"

    | Pconst_boolean true ->
      const_i32 (Int32.of_int_exn 1)

    | Pconst_boolean false ->
      const_i32 (Int32.of_int_exn 0)

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
      local_get var_sym.id_in_scope i32

    | Texp_constant cnst ->
      codegen_constant env cnst

    | Texp_call call ->
      let open Waterlang_typing.Typedtree in
      let callee_name = match call.tcallee.texp_desc with
        | Texp_identifier sym ->
          sym.name

        | _ -> failwith "unreachable"
      in
      let params =
        call.tcall_params
        |> List.map ~f:(codegen_expression env)
        |> List.to_array
      in
      call_ callee_name params i32

    | _ ->
      unreachable_exp()

  and codegen_function env function_ =
    let parms_type params =
      let open Typedtree in
      let { tparams_content; _; } = params in
      let params_arr = List.to_array tparams_content in
      let types_arr = Array.map ~f:(fun _ -> i32) params_arr in
      C_bindings.make_ty_multiples types_arr
    in

    let params_ty = parms_type function_.tfun_params in

    let { Typedtree. tfun_body; _; } = function_ in
    let exp =
      match tfun_body with
      | Typedtree.Tfun_block_body block ->
        begin
          let { Typedtree. tblk_body; _; } = block in
          List.iter
            ~f:(codegen_statements env)
            tblk_body
          ;
          Option.value_exn env.last_expr
        end

      | Typedtree.Tfun_expression_body expr ->
        codegen_expression env expr
    in

    let id = function_.tfun_id in
    let id_name = id.name in
    (* let sym = function_.tfun_id in
    let ret_ty =
      match sym.def_type with
      | Core_type.TypeValue.Function fun_ty ->
        fun_ty.tfun_ret
      | _ -> failwith "not a function"
    in *)
    let _fun = Dsl.function_ ~name:id_name ~params_ty ~ret_ty:i32 ~vars_ty:[| |] ~content:exp in
    let _ = export_function id_name id_name in
    ()

  and codegen_program env (program: Program.t) =
    let { Program. tree } = program in
    let { Typedtree. tprogram_statements } = tree in
    List.iter ~f:(codegen_statements env) tprogram_statements;

    if Codegen_env.needs_allocator env then (
      Allocator_facility.codegen_allocator_facility (module S);
    );

    C_bindings.set_memory env.module_ 1024 1024 "memory" [||] [||] [||] [||] false;
  
end

let codegen program config : string =
  let env = Codegen_env.create config in
  let module Cg = M(struct
      let m = env.module_
    end)
  in
  Cg.codegen_program env program;
  let str = C_bindings.module_emit_text env.module_ in
  str

let codegen_binary program env path : unit =
  let module Cg = M(struct
      let m = env.module_
    end)
  in
  Cg.codegen_program env program;
  C_bindings.module_emit_binary_to_file env.module_ path;
  let js_glue_content = Js_glue.dump_js_glue env in
  Out_channel.write_all (env.output_filename ^ ".js") ~data:js_glue_content
