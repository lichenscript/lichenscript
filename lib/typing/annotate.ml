open Core_kernel
open Core_type
open Typedtree
open Waterlang_parsing

let rec annotate_statement (env: Env.t) stmt =
  let { Ast. pstmt_loc; pstmt_desc; _; } = stmt in
  let tstmt_desc =
    match pstmt_desc with
    | Pstmt_class cls ->
      let cls = annotate_class env cls in
      (* let ty_val = Infer.infer_class cls in
      cls.tcls_id.value <- ty_val; *)
      Tstmt_class cls

    | Pstmt_expr expr ->
      Tstmt_expr (annotate_expression env expr)

    | Pstmt_semi expr ->
      Tstmt_semi (annotate_expression env expr)

    | Pstmt_function _fun ->
      Tstmt_function (annotate_function env _fun)

    | Pstmt_while {Ast. pwhile_test; pwhile_block; pwhile_loc; } ->
      let twhile_test = annotate_expression env pwhile_test in
      let twhile_block = annotate_block env pwhile_block in
      Tstmt_while {
        twhile_test;
        twhile_block;
        twhile_loc = pwhile_loc;
      }

    | Pstmt_binding binding ->
      Tstmt_binding (annotate_binding env binding)

    | Pstmt_block block ->
      Tstmt_block (annotate_block env block)

    | Pstmt_break id -> Tstmt_break id
    | Pstmt_contintue id -> Tstmt_continue id
    | Pstmt_debugger -> Tstmt_debugger
    | Pstmt_return expr_opt ->
      Tstmt_return (Option.map
        ~f:(function expr ->
          let annotated_expr = annotate_expression env expr in
          if Option.is_none (Env.return_type env) then (
            Env.set_return_type env (Some annotated_expr.texp_val);
          );
          annotated_expr
        )
        expr_opt)

    | Pstmt_empty -> Tstmt_empty

  in

  {
    tstmt_desc;
    tstmt_loc = pstmt_loc;
  }

and annotate_function env _function =
  let annotate_param env param =
    let {Ast. pparam_pat; pparam_init; pparam_loc; pparam_ty; pparam_rest; _ } = param in
    let tparam_pat: pattern = annotate_pattern env pparam_pat in
    let tparam_ty = pparam_ty
      |> Option.map ~f:(Infer.infer env)
      |> Option.value ~default:Core_type.TypeValue.Unknown
    in
    match tparam_pat.tpat_desc with
      | Tpat_symbol sym ->
        Core_type.VarSym.set_def_type sym tparam_ty;
    {
      tparam_pat;
      tparam_ty;
      tparam_init = Option.map ~f:(annotate_expression env) pparam_init;
      tparam_loc = pparam_loc;
      tparam_rest = pparam_rest;
    }
  in
  let { Ast. pfun_id; pfun_params; pfun_body; pfun_loc; pfun_return_ty; _; } = _function in
  let name = (Option.value_exn pfun_id).pident_name in
  let prev_scope = Env.peek_scope env in
  let var_sym = Scope.find_var_symbol prev_scope name in
  let scope = Scope.create ~prev:prev_scope prev_scope.id in
  Env.set_return_type env (Option.map ~f:(Infer.infer env) pfun_return_ty);
  Env.with_new_scope env scope (fun env ->
    match var_sym with
    | Some tfun_id ->
      begin
        let tfun_params =
          {
            tparams_content = List.map ~f:(annotate_param env) pfun_params.pparams_content;
            tparams_loc = pfun_params.pparams_loc;
          }
        in
        let tfun_body = match pfun_body with
          | Ast.Pfun_block_body block ->
            Tfun_block_body (annotate_block env block)

          | Ast.Pfun_expression_body expr ->
            Tfun_expression_body (annotate_expression env expr)

        in
        let return_ty =
          pfun_return_ty
          |> Option.map ~f:(Infer.infer env)
          |> Option.value ~default:TypeValue.Unit
        in
        let fun_ty =
          { TypeValue.
            tfun_params = List.map
              ~f:(fun param ->
                Typedtree.pp_pattern Format.str_formatter param.tparam_pat;
                let param_name = Format.flush_str_formatter () in
                (param_name, param.tparam_ty)
              )
              tfun_params.tparams_content;
            tfun_ret = return_ty;
          }
        in
        VarSym.set_def_type tfun_id (TypeValue.Function fun_ty);
        {
          tfun_id;
          tfun_params;
          tfun_body;
          tfun_assoc_scope = scope;
          tfun_loc = pfun_loc;
        }
      end

    | None ->
      begin
        let err = Type_error.make_error pfun_loc (Type_error.CannotFindName name) in
        Env.add_error env err;
        raise (Type_error.Error err)
      end
  )

and annotate_class env _class =
  let annotate_body env cls_body =
    let { Ast. pcls_body_elements; pcls_body_loc } = cls_body in
    let tcls_body_elements =
      List.map
        ~f:(function
        | Pcls_method _method ->
          begin
            let { Ast. pcls_method_visiblity; pcls_method_loc; _ } = _method in
            let tcls_method_visibility =
              Option.value ~default:Ast.Pvisibility_private pcls_method_visiblity
            in
            Tcls_method {
              tcls_method_visibility;
              tcls_method_loc = pcls_method_loc;
            }
          end

        | Pcls_property prop ->
          begin
            let { Ast.
              pcls_property_visiblity;
              pcls_property_loc;
              pcls_property_name;
              (* pcls_property_type; *)
              pcls_property_init;
              _;
            } = prop
            in
            let tcls_property_visibility =
              Option.value ~default:Ast.Pvisibility_private pcls_property_visiblity
            in
            let tcls_property_init = Option.map ~f:(annotate_expression env) pcls_property_init in
            Tcls_property {
              tcls_property_visibility;
              tcls_property_loc = pcls_property_loc;
              tcls_property_name = pcls_property_name;
              tcls_property_init;
            }
          end

        )
        pcls_body_elements
    in
    {
      tcls_body_elements;
      tcls_body_loc = pcls_body_loc;
    }
  in

  let { Ast. pcls_id; pcls_loc; pcls_body; _; } = _class in
  let id = Option.value_exn pcls_id in
  let scope = Env.peek_scope env in
  let type_sym_opt = Scope.find_type_symbol scope id.pident_name in
  match type_sym_opt with
  | Some tcls_id ->
    begin
      let tcls_body = annotate_body env pcls_body in
      {
        tcls_id;
        tcls_loc = pcls_loc;
        tcls_body;
      }
    end

  | None ->
    begin
      let err = Type_error.make_error pcls_loc (Type_error.CannotFindName id.pident_name) in
      Env.add_error env err;
      raise (Type_error.Error err)
    end

and annotate_block env block =
  let { Ast. pblk_body; pblk_loc } = block in
  let tblk_body = List.map ~f:(annotate_statement env) pblk_body in
  {
    tblk_body;
    tblk_loc = pblk_loc;
  }

and annotate_binding env binding =
  let { Ast. pbinding_kind; pbinding_loc; pbinding_ty; pbinding_pat; pbinding_init } = binding in
  let tbinding_init = annotate_expression env pbinding_init in
  let tbinding_ty: TypeValue.t option = Option.map ~f:(Infer.infer env) pbinding_ty in
  let tbinding_pat = annotate_pattern env pbinding_pat in

  Typedtree.(match tbinding_pat.tpat_desc with
  | Tpat_symbol sym ->
    match tbinding_ty with
    | Some t ->
      sym.def_type <- t

    | None ->
      sym.def_type <- tbinding_init.texp_val

  );

  {
    tbinding_kind = pbinding_kind;
    tbinding_loc = pbinding_loc;
    tbinding_ty;
    tbinding_pat;
    tbinding_init;
  }

and annotate_pattern env pat =
  let {Ast. ppat_desc; ppat_loc } = pat in
  let tpat_desc =
    match ppat_desc with
    | Ast.Ppat_identifier id ->
      begin
        let current_scope = Env.peek_scope env in
        let sym = Scope.create_var_symbol current_scope id.pident_name in
        Tpat_symbol sym
      end
  in
  {
    tpat_desc;
    tpat_loc = ppat_loc;
  }

and annotate_constant (env: Env.t) (cnst: Waterlang_parsing.Ast.constant) =
  let open Waterlang_parsing.Ast in
  match cnst with
  | Pconst_integer _ ->
    (cnst, Env.ty_i32 env)

  | Pconst_char _ ->
    (cnst, Env.ty_char env)

  | Pconst_float _ ->
    (cnst, Env.ty_f32 env)

  | Pconst_string _ ->
    (cnst, Env.ty_string env)

  | Pconst_boolean _ ->
    (cnst, Env.ty_boolean env)

and annotate_expression (env: Env.t) expr =
  let { Ast. pexp_desc; pexp_loc; _; } = expr in
  let (texp_desc, ty_val) =
    match pexp_desc with
    | Pexp_constant cnst ->
      let (cnst, ty_val) = annotate_constant env cnst in
      (Texp_constant cnst, TypeValue.Ctor ty_val)

    | Pexp_identifier id ->
      begin
        let current_scope = Env.peek_scope env in
        let var_sym_opt = Scope.find_var_symbol current_scope id.pident_name in
        match var_sym_opt with
        | Some sym ->
          (Texp_identifier sym, sym.def_type)

        | None ->
          begin
            let err = Type_error.make_error pexp_loc (Type_error.CannotFindName id.pident_name) in
            Env.add_error env err;
            raise (Type_error.Error err)
          end
      end

    | Pexp_throw expr ->
      let t = annotate_expression env expr in
      (Texp_throw t, TypeValue.Unknown) 

    | Pexp_lambda _ -> (Texp_lambda, TypeValue.Unknown)

    | Pexp_if { Ast. pif_test; pif_consequent; pif_alternative; pif_loc; } ->
      let tif_test = annotate_expression env pif_test in
      let tif_consequent = annotate_statement env pif_consequent in
      let tif_alternative = Option.map ~f:(annotate_statement env) pif_alternative in
      (Texp_if {
        tif_test;
        tif_consequent;
        tif_alternative;
        tif_loc = pif_loc;
      }, TypeValue.Unknown)

    | Pexp_array arr ->
      (Texp_array (List.map ~f:(annotate_expression env) arr), TypeValue.Unknown)
    
    | Pexp_call call ->
      let {Ast. pcallee; pcall_params; pcall_loc } = call in
      let tcallee = annotate_expression env pcallee in
      let return_ty =
        match tcallee.texp_val with
        | TypeValue.Function fun_ -> fun_.tfun_ret
        | _ -> TypeValue.Unknown
      in
      let tcall_params = List.map ~f:(annotate_expression env) pcall_params in
      (Texp_call {
        tcallee;
        tcall_params;
        tcall_loc = pcall_loc;
      }, return_ty)

    | Pexp_member (expr, field) ->

      let expr = annotate_expression env expr in

      let add_cannot_read_name_error name =
          let spec = Type_error.CannotReadMember(name, expr.texp_val) in
          let err = { Type_error. spec; loc = pexp_loc } in
          Env.add_error env err;
          raise (Type_error.Error err)
      in

      let open TypeValue in
      (match expr.texp_val with
      | Ctor { TypeSym. spec = Module_ mod_; _; } ->
        let name = field.pident_name in
        let field_type_opt = PropsMap.find mod_.props name in
        (match field_type_opt with
        | Some field_type ->
          (Texp_member(expr, field), field_type)
        | None -> add_cannot_read_name_error field.pident_name)

      | _ -> 
        add_cannot_read_name_error field.pident_name)

    | Pexp_unary (op, expr) ->
      let expr = annotate_expression env expr in
      (Texp_unary(op, expr), TypeValue.Unknown)

    | Pexp_binary (op, left, right) ->
      let left = annotate_expression env left in
      let right = annotate_expression env right in
      let ty_val = TypeValue.Ctor (Env.ty_i32 env) in
      (Texp_binary(op, left, right), ty_val)

    | Pexp_update (op, expr, prefix) ->
      let expr = annotate_expression env expr in
      (Texp_update(op, expr, prefix), TypeValue.Unknown)

  in
  {
    texp_desc;
    texp_loc = pexp_loc;
    texp_val = ty_val;
  }

(**
 * pre-scan symbol of root-level definitions of
 * class/function/struct/enum
 *)
let pre_scan_definitions env program =
  let { Ast. pprogram_statements; _; } = program in

  let find_or_add_type_sym name loc =
    let scope = Env.peek_scope env in
    let type_sym = Scope.find_type_symbol scope name in
    match type_sym with
    | Some _ ->
      begin
        let err = Type_error.make_error loc (Type_error.Redefinition name) in
        Env.add_error env err
      end

    | None ->
      begin
        let open Core_type in
        let sym = TypeSym.create ~scope_id:scope.id name TypeSym.Primitive in
        Scope.insert_type_symbol scope sym
      end
  in

  let find_or_add_var_sym name loc =
    let scope = Env.peek_scope env in
    let var_sym = Scope.find_var_symbol scope name in
    match var_sym with
    | Some _ ->
      begin
        let err = Type_error.make_error loc (Type_error.Redefinition name) in
        Env.add_error env err
      end

    | None ->
      let _ = Scope.create_var_symbol scope name in
      ()

  in

  List.iter
    ~f:(fun stmt ->
      let { Ast. pstmt_desc; _; } = stmt in
      match pstmt_desc with
      | Pstmt_class cls ->
        begin
          let { Ast. pcls_id; pcls_loc; _; } = cls in
          let id = Option.value_exn pcls_id in
          find_or_add_type_sym id.pident_name pcls_loc;
          find_or_add_var_sym id.pident_name pcls_loc
        end

      | Pstmt_function _fun ->
        begin
          let { Ast. pfun_id; pfun_loc; _; } = _fun in
          let id = Option.value_exn pfun_id in
          find_or_add_type_sym id.pident_name pfun_loc;
          find_or_add_var_sym id.pident_name pfun_loc
        end

      | _ -> ()
    )
    pprogram_statements

let annotate env (program: Ast.program) =
  pre_scan_definitions env program;
  let { Ast. pprogram_statements; _; } = program in
  let tprogram_statements =
    List.map ~f:(annotate_statement env) pprogram_statements
  in
  {
    tprogram_statements;
  }
