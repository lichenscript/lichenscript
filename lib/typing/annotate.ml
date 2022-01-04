open Core_kernel
open Core_type
open Waterlang_parsing

module T = Typedtree

let rec annotate_statement env (stmt: Ast.Statement.t) =
  let open Ast.Statement in
  let { loc; spec = parser_spec; attributes; _; } = stmt in
  let spec =
    match parser_spec with
    | Class cls ->
      let cls = annotate_class env cls in
      (* let ty_val = Infer.infer_class cls in
      cls.tcls_id.value <- ty_val; *)
      T.Statement.Class cls

    | Module { mod_visibility; mod_name } ->
      T.Statement.Module { mod_visibility; mod_name }

    | Expr expr ->
      T.Statement.Expr (annotate_expression env expr)

    | Semi expr ->
      T.Statement.Semi (annotate_expression env expr)

    | Function_ _fun ->
      T.Statement.Function_ (annotate_function env _fun)

    | While { while_test; while_block; while_loc; } ->
      let while_test = annotate_expression env while_test in
      let while_block = annotate_block env while_block in
      T.Statement.While {
        while_test;
        while_block;
        while_loc = while_loc;
      }

    | Binding binding ->
      T.Statement.Binding (annotate_binding env binding)

    | Block block ->
      T.Statement.Block (annotate_block env block)

    | Break id -> T.Statement.Break id
    | Contintue id -> T.Statement.Continue id
    | Debugger -> T.Statement.Debugger
    | Return expr_opt ->
      T.Statement.Return (Option.map
        ~f:(function expr ->
          let annotated_expr = annotate_expression env expr in
          if Option.is_none (Env.return_type env) then (
            Env.set_return_type env (Some annotated_expr.val_);
          );
          annotated_expr
        )
        expr_opt)

    | EnumDecl enum ->
      begin
        T.Statement.EnumDecl enum
      end

    | Decl decl ->
      let open Ast.Declare in
      let { spec; loc; } = decl in
      (match spec with
      | Function_ signature ->
        let prev_scope = Env.peek_scope env in 
        let scope = Scope.create ~prev:prev_scope prev_scope.id in
        Env.with_new_scope env scope (fun env ->
          let annotated_header = annotate_function_header env signature in
          let spec = T.Declare.Function_ annotated_header in
          T.Statement.Decl {
            spec;
            loc;
          })
        )

    | Empty -> T.Statement.Empty

  in

  { T.Statement.
    spec;
    loc;
    attributes;
  }

and annotate_function_header env (header: Ast.Function.header) =
  let open Ast.Function in
  let annotate_param env param =
    let { param_pat; param_init; param_loc; param_ty; param_rest; _ } = param in
    let param_pat: T.Pattern.t = annotate_pattern env param_pat in
    let param_ty = param_ty
      |> Option.map ~f:(Infer.infer env)
      |> Option.value ~default:Core_type.TypeValue.Unknown
    in
    match param_pat.spec with
      | T.Pattern.Symbol sym ->
        Core_type.VarSym.set_def_type sym param_ty;
    { T.Function.
      param_pat;
      param_ty;
      param_init = Option.map ~f:(annotate_expression env) param_init;
      param_loc = param_loc;
      param_rest = param_rest;
    }
  in
  let { id; params; header_loc; return_ty; _; } = header in
  let name = (Option.value_exn id).pident_name in
  let prev_scope: Scope.t = Option.value_exn ((Env.peek_scope env).prev) in
  let var_sym = Scope.find_var_symbol prev_scope name in
  match var_sym with
  | Some tfun_id ->
    begin
      let params =
        { T.Function.
          params_content = List.map ~f:(annotate_param env) params.params_content;
          params_loc = params.params_loc;
        }
      in
      let return_ty =
        return_ty
        |> Option.map ~f:(Infer.infer env)
        |> Option.value ~default:TypeValue.Unit
      in
      let fun_ty =
        { TypeValue.
          tfun_params = List.map
            ~f:(fun param ->
              T.Pattern.pp Format.str_formatter param.param_pat;
              let param_name = Format.flush_str_formatter () in
              (param_name, param.param_ty)
            )
            params.params_content;
          tfun_ret = return_ty;
        }
      in
      VarSym.set_def_type tfun_id (TypeValue.Function fun_ty);
      { T.Function.
        id = tfun_id;
        params;
      }
    end
  | None ->
    begin
      let err = Type_error.make_error header_loc (Type_error.CannotFindName name) in
      Env.add_error env err;
      raise (Type_error.Error err)
    end


and annotate_function env (_function: Ast.Function.t) =
  let open Ast.Function in
  let { header; body; loc;  _; } = _function in
  let { return_ty; _ } = header in
  let prev_scope = Env.peek_scope env in
  let scope = Scope.create ~prev:prev_scope prev_scope.id in
  Env.set_return_type env (Option.map ~f:(Infer.infer env) return_ty);
  Env.with_new_scope env scope (fun env ->
    let annotated_header = annotate_function_header env header in
    let body = match body with
      | Fun_block_body block ->
        T.Function.Fun_block_body (annotate_block env block)

      | Fun_expression_body expr ->
        T.Function.Fun_expression_body (annotate_expression env expr)

    in
    { T.Function.
      header = annotated_header;
      body;
      assoc_scope = scope;
      loc;
    }

  )

and annotate_class env _class =
  let open Ast.Statement in
  let annotate_body env cls_body =
    let { cls_body_elements; cls_body_loc } = cls_body in
    let cls_body_elements =
      List.map
        ~f:(function
        | Cls_method _method ->
          begin
            let { cls_method_visiblity; cls_method_loc; _ } = _method in
            let cls_method_visibility =
              Option.value ~default:Ast.Pvisibility_private cls_method_visiblity
            in
            T.Statement.Cls_method {
              cls_method_visibility;
              cls_method_loc;
            }
          end

        | Cls_property prop ->
          begin
            let {
              cls_property_visiblity;
              cls_property_loc;
              cls_property_name;
              (* pcls_property_type; *)
              cls_property_init;
              _;
            } = prop
            in
            let cls_property_visibility =
              Option.value ~default:Ast.Pvisibility_private cls_property_visiblity
            in
            let cls_property_init = Option.map ~f:(annotate_expression env) cls_property_init in
            T.Statement.Cls_property {
              cls_property_visibility;
              cls_property_loc = cls_property_loc;
              cls_property_name = cls_property_name;
              cls_property_init;
            }
          end

        )
        cls_body_elements
    in
    { T.Statement.
      cls_body_elements;
      cls_body_loc = cls_body_loc;
    }
  in

  let { cls_id; cls_loc; cls_body; _; } = _class in
  let id = Option.value_exn cls_id in
  let scope = Env.peek_scope env in
  let type_sym_opt = Scope.find_type_symbol scope id.pident_name in
  match type_sym_opt with
  | Some cls_id ->
    begin
      let cls_body: T.Statement.class_body = annotate_body env cls_body in
      { T.Statement.
        cls_id;
        cls_loc;
        cls_body;
      }
    end

  | None ->
    begin
      let err = Type_error.make_error cls_loc (Type_error.CannotFindName id.pident_name) in
      Env.add_error env err;
      raise (Type_error.Error err)
    end

and annotate_block env block =
  let open Ast.Block in
  let { body; loc } = block in
  let body = List.map ~f:(annotate_statement env) body in
  let val_ =
    match List.last body with
    | Some { Typedtree.Statement. spec = Expr expr; _;} ->
      expr.val_

    | _ -> TypeValue.Unit
  in
  { T.Block.
    body;
    loc;
    val_;
  }

and annotate_binding env (binding: Ast.Statement.var_binding) =
  let open Ast.Statement in
  let { binding_kind; binding_loc; binding_ty; binding_pat; binding_init } = binding in
  let binding_init = annotate_expression env binding_init in
  let binding_ty: TypeValue.t option = Option.map ~f:(Infer.infer env) binding_ty in
  let binding_pat = annotate_pattern env binding_pat in

  T.Pattern.(
    match binding_pat.spec with
    | Symbol sym ->
      (match binding_ty with
      | Some t ->
        sym.def_type <- t

      | None ->
        sym.def_type <- binding_init.val_)

  );

  { T.Statement.
    binding_kind;
    binding_loc;
    binding_ty;
    binding_pat;
    binding_init;
  }

and annotate_pattern ?(def=true) env (pat: Ast.Pattern.t) =
  let open Ast.Pattern in
  let { spec; loc } = pat in
  let spec =
    match spec with
    | Identifier id ->
      begin
        let current_scope = Env.peek_scope env in
        let sym =
          if def then
            Scope.create_var_symbol current_scope id.pident_name
          else
            Option.value_exn (Scope.find_var_symbol current_scope id.pident_name)
        in
        T.Pattern.Symbol sym
      end
  in
  { T.Pattern.
    spec;
    loc;
  }

and annotate_constant env (cnst: Ast.Literal.t) =
  let open Ast.Literal in
  match cnst with
  | Integer _ ->
    (cnst, Env.ty_i32 env)

  | Char _ ->
    (cnst, Env.ty_char env)

  | Float _ ->
    (cnst, Env.ty_f32 env)

  | String _ ->
    (cnst, Env.ty_string env)

  | Boolean _ ->
    (cnst, Env.ty_boolean env)

and annotate_expression (env: Env.t) expr =
  let open Ast.Expression in
  let { spec; loc; _; } = expr in
  let (spec, ty_val) =
    match spec with
    | Constant cnst ->
      let (cnst, ty_val) = annotate_constant env cnst in
      (T.Expression.Constant cnst, TypeValue.Ctor ty_val)

    | Identifier id ->
      begin
        let current_scope = Env.peek_scope env in
        let var_sym_opt = Scope.find_var_symbol current_scope id.pident_name in
        match var_sym_opt with
        | Some sym ->
          (T.Expression.Identifier sym, sym.def_type)

        | None ->
          begin
            let err = Type_error.make_error loc (Type_error.CannotFindName id.pident_name) in
            Env.add_error env err;
            raise (Type_error.Error err)
          end
      end

    | Lambda _ -> (T.Expression.Lambda, TypeValue.Unknown)

    | If { if_test; if_consequent; if_alternative; if_loc; } ->
      let if_test = annotate_expression env if_test in
      let if_consequent = annotate_statement env if_consequent in
      let if_alternative = Option.map ~f:(annotate_statement env) if_alternative in
      (T.Expression.If {
        if_test;
        if_consequent;
        if_alternative;
        if_loc;
      }, TypeValue.Unknown)

    | Array arr ->
      (T.Expression.Array
        (List.map ~f:(annotate_expression env) arr),
        TypeValue.Unknown)
    
    | Call call -> annotate_call env call

    | Member (expr, field) ->

      let expr = annotate_expression env expr in

      let add_cannot_read_name_error name =
          let spec = Type_error.CannotReadMember(name, expr.val_) in
          let err = { Type_error. spec; loc; } in
          Env.add_error env err;
          raise (Type_error.Error err)
      in

      let open TypeValue in
      (match expr.val_ with
      | Ctor { TypeSym. spec = Module_ mod_; _; } ->
        let name = field.pident_name in
        let field_type_opt = PropsMap.find mod_.props name in
        (match field_type_opt with
        | Some field_type ->
          (T.Expression.Member(expr, field), field_type)
        | None -> add_cannot_read_name_error field.pident_name)

      | _ -> 
        add_cannot_read_name_error field.pident_name)

    | Unary (op, expr) ->
      let expr = annotate_expression env expr in
      (T.Expression.Unary(op, expr), TypeValue.Unknown)

    | Binary (op, left, right) ->
      let left = annotate_expression env left in
      let right = annotate_expression env right in
      let ty_val = TypeValue.Ctor (Env.ty_i32 env) in
      (T.Expression.Binary(op, left, right), ty_val)

    | Update (op, expr, prefix) ->
      let expr = annotate_expression env expr in
      (T.Expression.Update(op, expr, prefix), TypeValue.Unknown)

    | Assign (left, right) ->
      let left' = annotate_pattern ~def:false env left in
      let right' = annotate_expression env right in
      (T.Expression.Assign(left', right'), right'.val_)

    | Block blk ->
      let blk' = annotate_block env blk in
      ((T.Expression.Block blk'), blk'.val_)

  in
  { T.Expression.
    spec;
    loc;
    val_ = ty_val;
  }

and annotate_call env (call: Ast.Expression.call) =
  let open Ast.Expression in
  let rec cast_member_to_callee (expr: T.Expression.t) props =
    match expr.spec with
    | T.Expression.Identifier id ->
      let (ty, props) = props
        |> List.rev
        |> List.fold_map
          ~init:id.def_type
          ~f:(fun acc id ->
            let open Core_type.TypeValue in
            let open Core_type.TypeSym in
            let open Identifier in
            let ty = match acc with
              | Ctor { spec = Module_ mod_; _; } ->
                let name = id.pident_name in
                let prop_opt = Core_type.PropsMap.find mod_.props name in
                Option.value_exn prop_opt

              | _ -> Unknown
            in
            let prop = `Property id.pident_name  in
            ty, prop
          )
      in
      let callee_item = (id, props) in
      (callee_item, ty)

    | T.Expression.Member(new_expr, id) ->
      cast_member_to_callee new_expr (id::props)

    | _ ->
      let spec = Type_error.NotCallable expr.val_ in
      let err = Type_error.make_error expr.loc spec in
      Env.add_error env err;
      raise (Type_error.Error err)

  and cast_expr_to_callee (expr: T.Expression.t): T.Expression.callee =
    let (spec, callee_ty) = cast_member_to_callee expr [] in
    { T.Expression.
      callee_spec = spec;
      callee_loc = expr.loc;
      callee_ty;
    }

  in
  let { callee; call_params; call_loc } = call in
  let tcallee = annotate_expression env callee in
  let return_ty =
    match tcallee.val_ with
    | TypeValue.Function fun_ -> fun_.tfun_ret
    | _ -> TypeValue.Unknown
  in
  let call_params = List.map ~f:(annotate_expression env) call_params in
  (T.Expression.Call {
    callee = cast_expr_to_callee tcallee;
    call_params;
    call_loc = call_loc;
  }, return_ty)

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
    | Some found ->
      begin
        let err = Type_error.make_error loc (Type_error.Redefinition name) in
        Env.add_error env err;
        found
      end

    | None ->
      begin
        let open Core_type in
        let sym = TypeSym.create ~scope_id:scope.id name TypeSym.Primitive in
        Scope.insert_type_symbol scope sym;
        sym
      end
  in

  let find_or_add_var_sym ?(spec=Core_type.VarSym.Internal) name loc =
    let scope = Env.peek_scope env in
    let var_sym = Scope.find_var_symbol scope name in
    match var_sym with
    | Some found ->
      begin
        let err = Type_error.make_error loc (Type_error.Redefinition name) in
        Env.add_error env err;
        found
      end

    | None ->
      Scope.create_var_symbol ~spec scope name

  in

  List.iter
    ~f:(fun stmt ->
      let open Ast.Statement in
      let { spec; attributes; _; } = stmt in
      match spec with
      | Class cls ->
        begin
          let { cls_id; cls_loc; _; } = cls in
          let id = Option.value_exn cls_id in
          let _ = find_or_add_type_sym id.pident_name cls_loc in
          let _ = find_or_add_var_sym id.pident_name cls_loc in
          ()
        end

      | Function_ _fun ->
        begin
          let open Ast.Function in
          let { header; loc; _; } = _fun in
          let { id; _; } = header in
          let id = Option.value_exn id in
          let _ = find_or_add_type_sym id.pident_name loc in
          let _  = find_or_add_var_sym id.pident_name loc in
          ()
        end

      | Decl declare ->
        begin
          let open Ast.Declare in
          let { spec; loc; _ } = declare in
          match spec with
          | Function_ signature ->
            let external_attrib = List.find
              ~f:Ast.(fun attr -> String.equal attr.attr_name.txt "external")
              attributes
            in
            Option.iter
              ~f:(fun attr ->
                match attr.attr_payload with
                | [ external_name ] ->
                  let spec = Core_type.VarSym.ExternalMethod external_name in
                  let id = Option.value_exn signature.id in
                  let _ = find_or_add_type_sym id.pident_name loc in
                  let _ = find_or_add_var_sym ~spec id.pident_name loc in
                  ()

                | _ -> failwith "not implemented 2"
              )
              external_attrib

        end

      | EnumDecl enum_decl ->
        begin
          let open Ast.Enum in
          let { name; loc; _; } = enum_decl in
          let spec: Core_type.VarSym.spec = 
            let a_list =
              List.mapi
                ~f:(fun index member ->
                  let field =
                    { Core_type.VarSym.
                      enum_id = index;
                    }
                  in
                  (member.member_name.pident_name, field)
                )
                enum_decl.members
            in
            let enum_map = Core_type.PropsMap.of_alist a_list in
            let unwrapped_enum =
              match enum_map with
              | `Ok emap -> emap
              | _ -> failwith "unexpected"
            in
            Core_type.VarSym.Enum unwrapped_enum
          in
          let _ = find_or_add_type_sym name.pident_name loc in
          let _  = find_or_add_var_sym ~spec name.pident_name loc in
          ()
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
  let root_scope = Env.root_scope env in
  { T.
    tprogram_statements;
    root_scope;
  }
