open Core_kernel
open Core_type
open Typedtree

let type_assinable left right =
  let open TypeValue in
  match (left, right) with
  | (Any, _)
  | (_, Any)
  | (Unknown, Unknown) -> true
  | (Ctor left_sym, Ctor right_sym) ->
    if phys_equal left_sym right_sym then
      true
    else
      false

  | _ ->
    false

let unwrap_pattern_type _env pat =
  let open Typedtree.Pattern in
  match pat.spec with
  | Typedtree.Pattern.Symbol _var_sym ->
    TypeValue.Any

let rec check_statement env statement =
  let open Typedtree.Statement in
  let { spec; loc; _; } = statement in
  match spec with
  | Class cls ->
    check_class env cls

  | Expr expr
  | Semi expr ->
    check_expression env expr

  | Function_ _fun ->
    check_function env _fun

  | While _while ->
    begin
      let { while_test; while_block; _; } = _while in
      check_expression env while_test;
      check_block env while_block
    end

  | Binding binding -> 
    check_binding env binding

  | Block blk ->
    check_block env blk

  | Break _
  | Continue _
  | Debugger -> ()
  | Return expr_opt -> 
    let expected = Option.value ~default:TypeValue.Unit (Env.return_type env) in
    (match expr_opt with
    | Some expr ->
      check_expression env expr;
      check_returnable env loc expected expr.val_

    | None -> 
      check_returnable env loc expected TypeValue.Unit

    )

  | EnumDecl _
  | Decl _
  | Empty -> ()

and check_class env cls =
  let open Typedtree.Statement in
  let { cls_body; _; } = cls in
  let { cls_body_elements; _; } = cls_body in
  List.iter
    ~f:(function
    | Cls_method _ ->
      ()

    | Cls_property prop ->
      let { cls_property_init; _; } = prop in
      Option.iter
        ~f:(fun expr ->
          check_expression env expr;
        )
        cls_property_init

    )
    cls_body_elements

and check_function (env: Env.t) _fun =
  let open Typedtree.Function in
  let { body;  _; } = _fun in
  match body with
  | Fun_block_body blk -> 
    check_block env blk;

  | Fun_expression_body expr ->
    check_expression env expr

and check_block env blk =
  let open Typedtree.Block in
  let { body; _ } = blk in
  List.iter ~f:(check_statement env) body

and check_binding env binding =
  let open Typedtree.Statement in
  let { binding_pat; binding_init; binding_loc; _ } = binding in
  let left_val = unwrap_pattern_type env binding_pat in
  check_expression env binding_init;
  check_assignable env binding_loc left_val binding_init.val_

and check_expression env (expr: Typedtree.Expression.t) =
  let open Typedtree.Expression in
  let { spec; loc; _; } = expr in
  match spec with
  | Constant _
  | Identifier _
  | Lambda
    -> ()

  | If _if ->
    begin
      let { if_test; if_consequent; if_alternative; _; } = _if in
      check_expression env if_test;
      check_statement env if_consequent;
      Option.iter ~f:(check_statement env) if_alternative
    end

  | Array arr ->
    List.iter
      ~f:(fun expr ->
        check_expression env expr;
      )
      arr
    ;

  | Call call ->
    let { callee; call_params; call_loc; _; } = call in
    check_callee env callee;
    let callee_type = callee.callee_ty in
    TypeValue.(
    match callee_type with
    | Function fun_ ->
      begin
        let result = List.iter2 call_params fun_.tfun_params
          ~f:(fun actual_param (name, def_param) ->
            check_passable env call_loc ~name ~def:def_param ~actual:actual_param.val_
          )
        in
        match result with
        | Unequal_lengths ->
          let err = Type_error.make_error call_loc (Type_error.ParamsMismatch callee_type) in
          Env.add_error env err

        | _ -> ()
      end

    | _ ->
      let err = Type_error.make_error call_loc (Type_error.NotCallable callee_type) in
      Env.add_error env err
    )

  | Member (expr, _field) ->
    check_expression env expr

  | Unary (_, expr) ->
    check_expression env expr

  | Binary (_, left, right) ->
    check_expression env left;
    check_expression env right

  | Update (_, exp, _) ->
    check_expression env exp

  | Assign (left, right) ->
    let left_val = unwrap_pattern_type env left in
    check_expression env right;
    check_assignable env loc left_val right.val_

  | Block blk ->
    check_block env blk

and check_callee _env (callee: Typedtree.Expression.callee) =
  match callee.callee_spec with
  | (_, []) -> ()

  | (sym, arr) ->
    let _ = List.fold
      ~init:sym.def_type
      ~f:(fun acc prop -> (match prop with
      | `Property prop_name -> (
        let open Core_type.TypeValue in
        let open Core_type.TypeSym in
        match acc with
        | Ctor { spec = Module_ mod_; _; } ->
          let prop_opt = Core_type.PropsMap.find mod_.props prop_name in
          Option.value_exn prop_opt

        | _ -> Unknown
      )
      | `Expr _expr -> failwith "not implement"))
      arr
    in ()

and check_assignable env loc left right =
  if not (type_assinable left right) then (
    let spec = Type_error.NotAssignable(left, right) in
    let err = {Type_error. loc; spec } in
    Env.add_error env err
  )

and check_returnable env loc expected actual =
  if not (type_assinable expected actual) then (
    let spec = Type_error.CannotReturn(expected, actual) in
    let err = {Type_error. loc; spec } in
    Env.add_error env err
  )

and check_passable env loc ~name ~(def: TypeValue.t) ~(actual: TypeValue.t) =
  if not (type_assinable def actual) then (
    let spec = Type_error.CannotPassParam(name, def, actual) in
    let err = {Type_error. loc; spec } in
    Env.add_error env err
  )

let type_check env program =
  let { tprogram_statements; _; } = program in
  List.iter ~f:(check_statement env) tprogram_statements
