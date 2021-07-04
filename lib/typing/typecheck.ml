open Core_kernel
open Core_type
open Typedtree
open Waterlang_parsing

let rec check_statement env statement =
  let { tstmt_desc; _; } = statement in
  match tstmt_desc with
  | Tstmt_class cls ->
    check_class env cls

  | Tstmt_expr expr
  | Tstmt_semi expr ->
    check_expression env expr

  | Tstmt_function _fun ->
    check_function env _fun

  | Tstmt_while _while ->
    begin
      let { twhile_test; twhile_block; _; } = _while in
      check_expression env twhile_test;
      check_block env twhile_block
    end

  | Tstmt_binding binding -> 
    check_binding env binding

  | Tstmt_block blk ->
    check_block env blk

  | Tstmt_break _
  | Tstmt_continue _
  | Tstmt_debugger -> ()
  | Tstmt_return expr_opt -> 
    Option.iter ~f:(check_expression env) expr_opt

  | Tstmt_empty -> ()

and check_class env cls =
  let { tcls_body; _; } = cls in
  let { tcls_body_elements; _; } = tcls_body in
  List.iter
    ~f:(function
    | Tcls_method _ ->
      ()

    | Tcls_property prop ->
      let { tcls_property_init; _; } = prop in
      Option.iter
        ~f:(fun expr ->
          check_expression env expr;
        )
        tcls_property_init

    )
    tcls_body_elements

and check_function (env: Env.t) _fun =
  let { tfun_body; tfun_return_ty; tfun_loc; _; } = _fun in
  let actual_return_ty =
    match tfun_body with
    | Tfun_block_body blk -> 
      begin
        check_block env blk;
        if List.is_empty blk.tblk_body then
          TypeValue.Unit
        else
          Option.value (Env.return_type env) ~default:(Core_type.TypeValue.Unit)
      end

    | Tfun_expression_body expr ->
      begin
        check_expression env expr;
        expr.texp_val
      end

  in
  check_assignable env tfun_loc tfun_return_ty actual_return_ty

and check_block env blk =
  let { tblk_body; _ } = blk in
  List.iter ~f:(check_statement env) tblk_body

and check_binding env binding =
  let { tbinding_pat; tbinding_init; tbinding_loc; _ } = binding in
  let { tpat_desc; _; } = tbinding_pat in
  let left_val =
    match tpat_desc with
    | Tpat_symbol _var_sym ->
      TypeValue.Any
  in
  check_expression env tbinding_init;
  check_assignable env tbinding_loc left_val tbinding_init.texp_val

and check_expression (env: Env.t) expr =
  let { texp_desc; _; } = expr in
  let type_value =
    match texp_desc with
    | Texp_constant cnst ->
      TypeValue.(
        match cnst with
        | Ast.Pconst_integer _ ->
          Ctor (Env.ty_i32 env)

        | Ast.Pconst_char _ ->
          Ctor (Env.ty_char env)

        | Ast.Pconst_string _ ->
          Ctor (Env.ty_string env)

        | Ast.Pconst_float _ ->
          Ctor (Env.ty_f32 env)

        | Ast.Pconst_boolean _ ->
          Ctor (Env.ty_boolean env)

      )

    | Texp_identifier _
    | Texp_lambda -> TypeValue.Any
    | Texp_throw expr ->
      check_expression env expr;
      TypeValue.Any

    | Texp_if _if ->
      begin
        let { tif_test; tif_consequent; tif_alternative; _; } = _if in
        check_expression env tif_test;
        check_statement env tif_consequent;
        Option.iter ~f:(check_statement env) tif_alternative;
        TypeValue.Any
      end

    | Texp_array arr ->
      List.iter
        ~f:(fun expr ->
          check_expression env expr;
        )
        arr
      ;

      TypeValue.Array TypeValue.Any

    | Texp_call call ->
      let { tcallee; tcall_params; tcall_loc; _; } = call in
      check_expression env tcallee;
      let callee_type = tcallee.texp_val in
      let _param_types =
        List.map
          ~f:(fun param ->
            check_expression env param;
            param.texp_val
          )
          tcall_params
      in
      TypeValue.(
      match callee_type with
      | Function fun_ty ->
        fun_ty.tfun_ret

      | _ ->
        let err = Type_error.make_error tcall_loc (Type_error.NotCallable callee_type) in
        Env.add_error env err;
        TypeValue.Any
      )

    | Texp_member (expr, _field) ->
      check_expression env expr;
      TypeValue.Any

    | Texp_unary (_, expr) ->
      check_expression env expr;
      TypeValue.Any

    | Texp_binary (_, left, right) ->
      check_expression env left;
      check_expression env right;
      TypeValue.Any

    | Texp_update (_, exp, _) ->
      check_expression env exp;
      TypeValue.Any

  in
  expr.texp_val <- type_value

and check_assignable env loc left right =
  let spec = match (left, right) with
  | (Any, _)
  | (_, Any)
  | (Unknown, Unknown) -> None
  | (Ctor left_sym, Ctor right_sym) ->
    if phys_equal left_sym right_sym then
      None
    else
      begin
        let spec = Type_error.NotAssignable(left, right) in
        Some spec
      end

  | _ ->
    let spec = Type_error.NotAssignable(left, right) in
    Some spec
  in
  match spec with
  | Some spec ->
    let err = {Type_error. loc; spec } in
    Env.add_error env err

  | None -> ()

let type_check env program =
  let { tprogram_statements; _; } = program in
  List.iter ~f:(check_statement env) tprogram_statements
