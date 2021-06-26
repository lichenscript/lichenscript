open Core_kernel
open Codegen_env
open Waterlang_typing

let rec codegen_statements env stat =
  let open Typedtree in
  let { tstmt_desc; _; } = stat in
  match tstmt_desc with
  | Tstmt_function fun_ ->
    env.last_expr <- None;
    codegen_function env fun_
  
  | Tstmt_expr expr ->
    let expr_result = codegen_expressions env expr in
    env.last_expr <- (Some expr_result)

  | Tstmt_return expr_opt ->
    let expr = Option.map
      ~f:(codegen_expressions env)
      expr_opt
    in
    let return_expr = C_bindings.make_exp_return env.module_ expr in
    env.last_expr <- (Some return_expr)

  | _ ->
    ()

and codegen_expressions env expr: C_bindings.exp =
  let open Typedtree in
  let { texp_desc; _; } = expr in
  match texp_desc with
  | Texp_binary(_op, left, right) ->
    begin
      let left = codegen_expressions env left in
      let right = codegen_expressions env right in
      let op = C_bindings.make_op_add_i32 () in
      C_bindings.make_exp_binary env.module_ op left right
    end

  | Texp_identifier var_sym ->
    let i32_ty = C_bindings.make_ty_int32 () in
    C_bindings.make_exp_local_get env.module_ var_sym.id_in_scope i32_ty

  | Texp_constant _ ->
    let lit = C_bindings.make_literal_i32 (Int32.of_int_exn 100) in
    C_bindings.make_exp_const env.module_ lit

  | _ ->
    C_bindings.make_exp_unrechable env.module_

and codegen_function env function_ =
  let i32_ty = C_bindings.make_ty_int32 () in
  let parms_type params =
    let open Typedtree in
    let { tparams_content; _; } = params in
    let params_arr = List.to_array tparams_content in
    let types_arr = Array.map ~f:(fun _ -> i32_ty) params_arr in
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
      codegen_expressions env expr
  in

  let id = function_.tfun_id in
  let id_name = id.name in
  let _fun = C_bindings.add_function env.module_ id_name params_ty i32_ty [| |] exp in
  let _ = C_bindings.add_function_export env.module_ id_name id_name in
  ()

and codegen_program env (program: Program.t) =
  let { Program. tree } = program in
  let { Typedtree. tprogram_statements } = tree in
  List.iter ~f:(codegen_statements env) tprogram_statements

let codegen program config : string =
  let env = Codegen_env.create config in
  codegen_program env program;
  let str = C_bindings.module_emit_text env.module_ in
  str

let codegen_binary program env path : unit =
  codegen_program env program;
  C_bindings.module_emit_binary_to_file env.module_ path;
  let js_glue_content = Js_glue.dump_js_glue env in
  Out_channel.write_all (env.output_filename ^ ".js") ~data:js_glue_content
