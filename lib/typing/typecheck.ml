(*
 * Type check deeply, check every expressions
 **)
open Core_kernel
(*
open Core_type
open Typedtree

module TypecheckEnv : sig

  type t

  val create: type_provider:Type_provider.provider -> open_domains:(string array list) -> Scope.t -> t

  val add_error:  t -> Type_error.t -> unit

  val return_type: t -> TypeValue.t option

  val errors: t -> Type_error.t list

  val resolve_open_domain: t -> string -> Core_type.VarSym.t option

  val peek_scope: t -> Scope.t

  val with_scope: t -> Scope.t -> (unit -> 'a) -> 'a
  
end = struct

  type t = {
    type_provider: Type_provider.provider;
    open_domains: string array list;
    mutable scope: Scope.t;
    mutable errors: Type_error.t list;
    mutable return_type: TypeValue.t option;
  }

  let create ~type_provider ~open_domains scope = {
    open_domains;
    type_provider;
    scope;
    errors = [];
    return_type = None;
  }

  let add_error env err =
    env.errors <- err::env.errors

  let return_type env = env.return_type

  let errors env = List.rev env.errors

  (* iterate the open domains *)
  let resolve_open_domain env name =
    List.fold_until  
      ~init:None
      ~f:(fun acc item ->
        let test = env.type_provider#resolve (item, [| name |]) in
        match test with
        | Some var ->
          Continue_or_stop.Stop (Some var)
        | None ->
          Continue_or_stop.Continue acc
      )
      env.open_domains
      ~finish:(fun acc -> acc)

  let peek_scope env = env.scope

  let with_scope env scope cb =
    let prev = env.scope in
    env.scope <- scope;
    let result = cb () in
    env.scope <- prev;
    result
  
end

let type_assinable left right =
  let open TypeValue in
  match (left, right) with
  | (Any, _)
  | (_, Any)
  | (Unknown, Unknown) -> true
  | (Ctor left_sym, Ctor right_sym) ->
    if TypeSym.(left_sym == right_sym) then
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
  let { spec; loc; attributes; } = statement in
  let spec = match spec with
  | Class cls ->
    Class (check_class env cls)

  | Module _ ->spec 

  | Expr expr -> Expr (check_expression env expr)
  | Semi expr -> Semi (check_expression env expr)

  | Function_ _fun ->
    Function_ (check_function env _fun)

  | While _while ->
    begin
      let { while_test; while_block; _ } = _while in
      let while_test = check_expression env while_test in
      let while_block = check_block env while_block in
      While { _while with while_test; while_block }
    end

  | Binding binding -> 
    Binding (check_binding env binding)

  | Block blk -> Block (check_block env blk)

  | Break _
  | Continue _
  | Debugger -> spec
  | Return expr_opt -> 
    let expected = Option.value ~default:TypeValue.Unit (TypecheckEnv.return_type env) in
    (match expr_opt with
    | Some expr ->
      let expr = check_expression env expr in
      check_returnable env loc expected expr.val_;
      Return (Some (expr))

    | None -> 
      check_returnable env loc expected TypeValue.Unit;
      Return None

    )

  | EnumDecl _
  | Decl _
  | Empty -> spec
  in
  { spec; loc; attributes; }

and check_class env cls  =
  let open Typedtree.Statement in
  let { cls_body; _; } = cls in
  let { cls_body_elements; _; } = cls_body in
  let cls_body_elements =
    List.map
    ~f:(function
    | Cls_method _method ->
      (Cls_method _method)

    | Cls_property prop ->
      let { cls_property_init; _; } = prop in
      let prop' = Option.map
        ~f:(fun expr ->
          check_expression env expr;
        )
        cls_property_init
      in
      Cls_property { prop with cls_property_init = prop' }

    )
    cls_body_elements
  in
  let cls_body = { cls_body with cls_body_elements } in
  { cls with cls_body }

and check_function (env: TypecheckEnv.t) _fun : Function.t =
  let open Typedtree.Function in
  let { body; assoc_scope; _ } = _fun in
  TypecheckEnv.with_scope env assoc_scope (fun () ->
    let body = match body with
      | Fun_block_body blk -> 
        Fun_block_body (check_block env blk)

      | Fun_expression_body expr ->
        Fun_expression_body (check_expression env expr)
    in
    { _fun with body; assoc_scope }
  )

and check_block env blk : Typedtree.Block.t =
  let open Typedtree.Block in
  let { body; loc; val_ } = blk in
  let body = List.map ~f:(check_statement env) body in
  { body; loc; val_ }

and check_binding env binding =
  let open Typedtree.Statement in
  let { binding_pat; binding_init; binding_loc; _ } = binding in
  let left_val = unwrap_pattern_type env binding_pat in
  let binding_init = check_expression env binding_init in
  check_assignable env binding_loc left_val binding_init.val_;
  { binding with binding_init }

and check_expression env (expr: Typedtree.Expression.t): Typedtree.Expression.t =
  let open Typedtree.Expression in
  let { spec; loc; _; } = expr in
  match spec with
  | Constant _
  | Identifier _
  | Lambda
    -> expr

  | UnresolvedIdentifier id -> (
    let _current_scope = TypecheckEnv.peek_scope env in
    let test = TypecheckEnv.resolve_open_domain env id.pident_name in
    match test with
    | Some sym ->
      { Expression.
        spec = Expression.Identifier sym;
        loc;
        val_ = sym.def_type;
      }

    | None ->
      let err = Type_error.make_error loc (Type_error.CannotFindName id.pident_name) in
      TypecheckEnv.add_error env err;
      expr

  )
  

  | If _if ->
    begin
      let { if_test; if_consequent; if_alternative; _; } = _if in
      let if_test = check_expression env if_test in
      let if_consequent = check_statement env if_consequent in
      let if_alternative = Option.map ~f:(check_statement env) if_alternative in
      let spec = If { _if with if_test; if_consequent; if_alternative } in
      { expr with spec }
    end

  | Array arr ->
    let spec = Array (
      List.map
      ~f:(fun expr ->
        check_expression env expr;
      )
      arr
    )
    in
    { expr with spec }

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
          TypecheckEnv.add_error env err;
          expr

        | _ -> expr
      end

    | _ ->
      let err = Type_error.make_error call_loc (Type_error.NotCallable callee_type) in
      TypecheckEnv.add_error env err;
      expr
    )

  | Member (expr, _field) ->
    let expr = check_expression env expr in
    let spec = Member(expr, _field) in
    { expr with spec }

  | Unary (op, expr) ->
    let expr = check_expression env expr in
    let spec = Unary (op, expr) in
    { expr with spec }

  | Binary (op, left, right) -> (
    let left = check_expression env left in
    let right = check_expression env right in
    let spec = Binary(op, left, right) in
    { expr with spec }
  )

  | Update (op, exp, prefix) ->
    let exp = check_expression env exp in
    let spec = Update (op, exp, prefix) in
    { expr with spec }

  | Assign (left, right) ->
    let left_val = unwrap_pattern_type env left in
    let right = check_expression env right in
    check_assignable env loc left_val right.val_;
    let spec = Assign (left, right) in
    { expr with spec }

  | Block blk ->
    let spec = Block (check_block env blk) in
    { expr with spec }

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
    TypecheckEnv.add_error env err
  )

and check_returnable env loc expected actual =
  if not (type_assinable expected actual) then (
    let spec = Type_error.CannotReturn(expected, actual) in
    let err = {Type_error. loc; spec } in
    TypecheckEnv.add_error env err
  )

and check_passable env loc ~name ~(def: TypeValue.t) ~(actual: TypeValue.t) =
  if not (type_assinable def actual) then (
    let spec = Type_error.CannotPassParam(name, def, actual) in
    let err = {Type_error. loc; spec } in
    TypecheckEnv.add_error env err
  )

let type_check_intern env program =
  let { tprogram_statements; _; } = program in
  let tprogram_statements = List.map ~f:(check_statement env) tprogram_statements in
  { program with tprogram_statements }

let type_check ?(type_provider=Type_provider.default_provider) ?(open_domains=[]) program =
  let env = TypecheckEnv.create ~type_provider ~open_domains program.root_scope in
  let program = type_check_intern env program in
  program, (TypecheckEnv.errors env) *)

module IntHash = Hashtbl.Make(Int)

let type_check _program =
  let visited_set = Hash_set.create (module Int) in
  let reversed_map = IntHash.create () in

  let no_deps = ref [] in
  let size = Type_env.size () in

  for i = 0 to (size - 1) do
    let node = Type_env.get_node i in
    let deps = node.deps in
    match deps with
    | [] -> (
      no_deps := i::(!no_deps)
    )
    | _ ->
      List.iter
        ~f:(fun dep ->
          match IntHash.find reversed_map dep with
          | Some exist ->
            IntHash.set reversed_map ~key:dep ~data:(i::exist)
          | None ->
            IntHash.set reversed_map ~key:dep ~data:[i]
        )
        deps
  done;

  let rec iterate_node node_id =
    if not (Hash_set.mem visited_set node_id) then (
      let node = Type_env.get_node node_id in
      node.check();
      Hash_set.add visited_set node_id;

      match IntHash.find reversed_map node_id with
      | None -> ()
      | Some arr -> (
        List.iter ~f:iterate_node arr
      )
    )
  in

  List.iter ~f:iterate_node !no_deps;

  []
