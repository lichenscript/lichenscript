(*
 * Convert TypedTree to C-op
 *)
open Core_kernel
open Lichenscript_parsing.Asttypes
open Lichenscript_parsing.Ast
open Lichenscript_typing
open Lichenscript_typing.Scope
open Lichenscript_typing.Typedtree

module TScope = struct
  (* Why name_map?
   * 1. The same variable(type_id) has different meaning in different scope,
   *    In a lambda expression, a captured value represents a value to this,
   *    but int outer scope, it's a local variable.
   *)
  type t = {
    name_map: (string, C_op.symbol) Hashtbl.t;
    raw: scope option;
    prev: t option;
  }

  let create scope =
    let name_map = Hashtbl.create (module String) in
    {
      name_map;
      raw = scope;
      prev = None;
    }

  let rec find_variable scope name =
    let find_in_prev () =
      match scope.prev with
      | Some prev_scope -> find_variable prev_scope name
      | None -> failwith (Format.sprintf "can not find variable %s" name)
    in
    match Hashtbl.find scope.name_map name with
    | Some v -> v
    | None -> find_in_prev ()

  let distribute_name current_scope name =
    let fun_name = "LCC_" ^ name in
    Hashtbl.set current_scope.name_map ~key:name ~data:(SymLocal fun_name);
    fun_name

  let set_name scope =
    Hashtbl.set scope.name_map
  
end

type cls_meta = {
  cls_id: int;
  cls_gen_name: string;
  (*
   * logical name -> realname
   *
   * only this class, does NOT includes ancester's
   *)
  cls_fields_map: (string, string) Hashtbl.t;

  (* all generating fields, including ancester's *)
  cls_fields: string list;
}

let create_cls_meta cls_id cls_gen_name cls_fields : cls_meta = {
  cls_id;
  cls_gen_name;
  cls_fields_map = Hashtbl.create (module String);
  cls_fields;
}

type current_fun_meta = {
  fun_name: string;
  used_name: string Hash_set.t;
  mutable def_local_names: string list;
}

let preserved_name = [ "ret"; "rt"; "this"; "argc"; "argv"; "t" ]

let create_current_fun_meta fun_name = {
  fun_name;
  used_name = Hash_set.of_list (module String) preserved_name;
  def_local_names = [];
}

type t = {
  ctx: Type_context.t;
  mutable scope: TScope.t;
  mutable tmp_vars_count: int;
  mutable main_function_name: string option;
  mutable class_inits: C_op.Decl.class_init list;
  mutable has_early_return: bool;

  (*
   * Some variables are local, but some are global,
   * such as a method of a class, the contructor of enum, etc
   *)
  global_name_map: (int, C_op.symbol) Hashtbl.t;

  cls_meta_map: (int, cls_meta) Hashtbl.t;

  (* for lambda generation *)
  mutable current_fun_name: current_fun_meta option;
  mutable lambdas: C_op.Decl.t list;
}

let push_scope env (scope: TScope.t) =
  let scope = {
    scope with
    prev = Some env.scope;
  } in
  env.scope <- scope

let pop_scope env =
  env.scope <- Option.value_exn env.scope.prev

let with_scope env scope cb =
  push_scope env scope;
  let result = cb env in
  pop_scope env;
  result

let find_variable env =
  TScope.find_variable env.scope

let should_var_captured variable =
  if !(variable.var_captured) then (
    match variable.var_kind with
    | Pvar_let -> true
    | _ -> false
  ) else
    false

type expr_result = {
  prepend_stmts: C_op.Stmt.t list;
  expr: C_op.Expr.t;
  append_stmts: C_op.Stmt.t list;
}

let create ctx =
  let scope = TScope.create None in
  let global_name_map = Hashtbl.create (module Int) in
  let cls_meta_map = Hashtbl.create (module Int) in
  {
    ctx;
    scope;
    tmp_vars_count = 0;
    main_function_name = None;
    class_inits = [];
    has_early_return = false;
    global_name_map;
    cls_meta_map;
    current_fun_name = None;
    lambdas = [];
  }

let get_local_var_name fun_meta realname ty_int =
  let name =
    if Hash_set.mem fun_meta.used_name realname then
      realname ^ "_" ^ (Int.to_string ty_int)
    else
      realname
  in
  Hash_set.add fun_meta.used_name name;
  name

let rec transform_declaration env decl =
  let open Declaration in
  let { spec; loc; _ } = decl in
  match spec with
  | Class cls -> (
    let specs = transform_class env cls in
    List.map ~f:(fun spec -> { C_op.Decl. spec; loc }) specs
  )
  | Function_ _fun -> (
    let specs = transform_function env _fun in
    let lambdas = env.lambdas in
    env.lambdas <- [];
    let bodys = List.map ~f:(fun spec -> { C_op.Decl. spec; loc }) specs in
    List.append lambdas bodys
  )

  | Enum enum -> (
    let specs = transform_enum env enum in
    List.map ~f:(fun spec -> { C_op.Decl. spec; loc }) specs
  )
  | Declare _
  | Import _ -> []

and distribute_name env =
  TScope.distribute_name env.scope

(*
 * 1. Scan the function firstly, find out all the lambda expression
 *    and captured variables
 * 2. Transform body
 *)
and transform_function env _fun =
  let open Function in
  let { body; comments; header; scope; _ } = _fun in
  let original_name, _ = header.name in

  env.current_fun_name <- Some (create_current_fun_meta original_name);
  env.has_early_return <- false;

  let fun_name = distribute_name env original_name in

  if String.equal original_name "main" then (
    env.main_function_name <- Some fun_name
  );

  let result = transform_function_impl env ~name:fun_name ~params:header.params ~scope ~body ~comments in

  env.current_fun_name <- None;

  [ result ]


and distribute_name_to_scope scope fun_meta local_vars : unit =
  let local_names =
    local_vars
    |> List.map
      ~f:(fun (var_name, variable) ->
        let gen_name = get_local_var_name fun_meta var_name variable.var_id in
        TScope.set_name
          scope
          ~key:var_name
          ~data:(SymLocal gen_name);

        gen_name
      )
  in
  fun_meta.def_local_names <- List.append fun_meta.def_local_names local_names

and transform_function_impl env ~name ~params ~body ~scope ~comments =
  let open Function in

  let fun_meta = Option.value_exn env.current_fun_name in
  let fun_scope = TScope.create (Some scope) in
  push_scope env fun_scope;

  let params_set = Hash_set.create (module String) in

  List.iteri
    ~f:(fun index { param_name; _ } ->
      let param_name, _ = param_name in
      Hash_set.add params_set param_name;
      (* Hashtbl.set env.name_map ~key:param_ty ~data:(get_local_var_name param_name param_ty) *)
      TScope.set_name fun_scope ~key:param_name ~data:(SymParam index)
    )
    params.params_content;

  let capturing_variables = scope#capturing_variables in
  Scope.CapturingVarMap.iteri
    ~f:(fun ~key:name ~data:idx ->
      TScope.set_name fun_scope ~key:name ~data:(SymLambda idx);
      Hash_set.add params_set name;
    )
    capturing_variables;

  (* filter all the params, get the "pure" local vars *)
  let local_vars =
    scope#vars
    |> List.filter ~f:(fun (name, _) -> not (Hash_set.mem params_set name))
  in

  distribute_name_to_scope fun_scope fun_meta local_vars;

  let max_tmp_value = ref env.tmp_vars_count in

  let before_stmts = env.tmp_vars_count in

  let stmts =
    List.map
      ~f:(fun stmt ->
        env.tmp_vars_count <- before_stmts;

        let result = transform_statement env stmt in

        if env.tmp_vars_count > !max_tmp_value then (
          max_tmp_value := env.tmp_vars_count
        );

        result
      )
      body.body
    |> List.concat
  in

  let cleanup_label =
    if env.has_early_return then
      [{ C_op.Stmt.
        spec = Label "cleanup";
        loc = Loc.none;
      }]
    else
      []
  in

  let cleanup =
    if (List.length local_vars) > 0 then (
      List.map
        ~f:(fun (name, _) ->
          { C_op.Stmt.
            spec = Release {
              spec = Ident (C_op.SymLocal name);
              loc = Loc.none;
            };
            loc = Loc.none;
          }
        )
        local_vars
    ) else [] in

  let generate_name_def () =
    let names = fun_meta.def_local_names in
    [{ C_op.Stmt.
      spec = VarDecl names;
      loc = Loc.none;
    }]
  in

  let def =
    if (List.length local_vars) > 0 then (
      generate_name_def ()
    ) else
      []
  in

  let new_body = {
    C_op.Block.
    body = List.concat [ def; stmts; cleanup_label; cleanup ];
    loc = body.loc;
  } in

  let t_fun = {
    C_op.Func.
    name;
    body = new_body;
    tmp_vars_count = !max_tmp_value;
    comments;
  } in

  pop_scope env;

  env.tmp_vars_count <- !max_tmp_value;

  C_op.Decl.Func t_fun;

and create_scope_and_distribute_vars env raw_scope =
  let scope = TScope.create (Some raw_scope) in
  let local_vars = raw_scope#vars in

  distribute_name_to_scope scope (Option.value_exn env.current_fun_name) local_vars;

  scope

and transform_statement ?ret env stmt =
  let open Statement in
  let { spec; loc; _ } = stmt in
  let transform_return_expr ?ret expr =
    let tmp = transform_expression env expr in
    let ret = Option.value ~default:"ret" ret in
    let assign = {
      C_op.Expr.
      spec = Assign(C_op.SymLocal ret, tmp.expr);
      loc;
    } in
    (*
     * It's returning the function directly, it's not need to retain,
     * because it's directly assining to "ret" variable.
     *
     * But it should retain if it's a normal block.
     *)
    let retain_expr =
      if String.equal ret "ret" then []
      else
        [{
          C_op.Stmt.
          spec = Retain { C_op.Expr. spec = Ident (C_op.SymLocal ret); loc;};
          loc;
        }]
    in
    let expr = {
      C_op.Stmt.
      spec = Expr assign;
      loc;
    } in
    List.concat [ tmp.prepend_stmts; [ expr ]; retain_expr; tmp.append_stmts ]
  in
  match spec with
  | Expr expr -> transform_return_expr ?ret expr

  | Semi expr -> (
    let tmp = transform_expression env expr in
    let expr = {
      C_op.Stmt.
      spec = Expr tmp.expr;
      loc;
    } in
    List.concat [ tmp.prepend_stmts; [ expr ]; tmp.append_stmts ]
  )

  | While { while_test; while_block; while_loc } -> (
    let transform_expression = transform_expression env while_test in

    let bodys = transform_block ?ret env while_block in
    let body = {
      C_op.Block.
      body = bodys;
      loc = while_block.loc;
    } in

    List.concat [
      transform_expression.prepend_stmts;
      [
        { C_op.Stmt.
          spec = While(transform_expression.expr, body);
          loc = while_loc;
        }
      ];
      transform_expression.append_stmts;
    ]
  )

  | Binding binding -> (
    let original_name =
      match binding.binding_pat with
      | { spec = Pattern.Symbol (name, _); _ } -> name
      | _ -> failwith "unrechable"
    in

    let scope = env.scope in

    let variable = Option.value_exn
      ~message:(Format.sprintf "%d:%d can not find %s" binding.binding_loc.start.line binding.binding_loc.start.column original_name)
      ((Option.value_exn scope.raw)#find_var_symbol original_name)
    in

    let name = TScope.find_variable scope original_name in
    let init_expr = transform_expression ~is_move:true env binding.binding_init in

    let assign_expr =
      if should_var_captured variable then (
        let init_expr = {  C_op.Expr.
          spec = NewRef init_expr.expr;
          loc = Loc.none;
        } in
        { C_op.Expr.
          spec = Assign(name, init_expr);
          loc = Loc.none;
        }
      ) else
        { C_op.Expr.
          spec = Assign(name, init_expr.expr);
          loc = Loc.none;
        }
    in

    List.concat [
      init_expr.prepend_stmts;
      [
        { C_op.Stmt.
          spec = Expr assign_expr;
          loc = binding.binding_loc;
        }
      ];
      init_expr.append_stmts;
    ]
  )

  | Break _ -> [
    { C_op.Stmt.
      spec = Break;
      loc;
    }
  ]

  | Continue _ -> [
    { C_op.Stmt.
      spec = Continue;
      loc;
    }
  ]
  | Debugger -> []

  | Return ret_opt -> (
    env.has_early_return <- true;
    let ret =
      Option.map ~f:(transform_return_expr ~ret:"ret") ret_opt
      |> Option.value ~default:[]
    in
    let goto_stmt = { C_op.Stmt.
      spec = Goto "cleanup";
      loc;
    } in
    List.append ret [goto_stmt]
  )

  | Empty -> []

and gen_release_temp id =
  { C_op.Stmt.
    spec = Release {
      C_op.Expr.
      spec = Temp id;
      loc = Loc.none;
    };
    loc = Loc.none;
  }

and auto_release_expr env ?(is_move=false) ~prepend_stmts ~append_stmts expr =
  let tmp_id = env.tmp_vars_count in
  env.tmp_vars_count <- env.tmp_vars_count + 1;

  let assign_expr = {
    C_op.Expr.
    spec = Assign(C_op.SymLocal ("t[" ^ (Int.to_string tmp_id) ^ "]"), expr);
    loc = Loc.none;
  } in

  let prepend_stmt = {
    C_op.Stmt.
    spec = Expr assign_expr;
    loc = Loc.none;
  } in

  prepend_stmts := List.append !prepend_stmts [prepend_stmt];

  let result = C_op.Expr.Temp tmp_id in

  if not is_move then (
    append_stmts := (gen_release_temp tmp_id)::!append_stmts
  );

  result

and transform_expression ?(is_move=false) env expr =
  let open Expression in
  let { spec; loc; ty_var; _ } = expr in
  let prepend_stmts = ref [] in
  let append_stmts = ref [] in
  let expr_spec =
    match spec with
    | Constant literal -> (
      let open Literal in
      match literal with
      | String(content, _, _) -> 
        auto_release_expr env ~is_move ~prepend_stmts ~append_stmts ({
          C_op.Expr.
          spec = NewString content;
          loc = Loc.none;
        })

      | Integer(content, _) ->
        C_op.Expr.NewInt content

      | _ ->
        failwith "unimplemented"
    )

    (*
     * 1. local variable
     * 2. enum constructor
     *)
    | Identifier (name, name_id) -> (
      let first_char = String.get name 0 in
      if Char.is_uppercase first_char then (
        let node_type = Type_context.deref_node_type env.ctx name_id in
        let open Core_type in
        match node_type with
        | TypeExpr.TypeDef({ TypeDef. spec = EnumCtor _; _ }, _) ->
          let sym = find_variable env name in
          C_op.Expr.ExternalCall(sym, None, [])

        | _ -> failwith (Format.sprintf "unknown identifier: %s" name)
      ) else (
        let variable_opt = (Option.value_exn env.scope.raw)#find_var_symbol name in
        let variable = Option.value_exn variable_opt in
        let sym = find_variable env name in
        if should_var_captured variable then (
          C_op.Expr.GetRef sym
        ) else
          C_op.Expr.Ident sym
      )
    )

    | Lambda lambda_content -> (
      let parent_scope = env.scope in
      let fun_meta = Option.value_exn ~message:"current function name not found" env.current_fun_name in
      let lambda_fun_name = "LCC_" ^ fun_meta.fun_name ^ "_lambda_" ^ (Int.to_string ty_var) in

      let capturing_variables = lambda_content.lambda_scope#capturing_variables in

      let capturing_names = Array.create ~len:(Scope.CapturingVarMap.length capturing_variables) (C_op.SymLocal "<unexpected>") in

      (* pass the capturing values into the deeper scope *)
      Scope.CapturingVarMap.iteri
      ~f:(fun ~key ~data -> 
        let name = TScope.find_variable parent_scope key in
        Array.set capturing_names data name
      )
      capturing_variables;

      let lambda = transform_lambda env ~lambda_gen_name:lambda_fun_name lambda_content ty_var in

      env.lambdas <- lambda::env.lambdas;

      let tmp = {
        C_op.Expr.
        spec = NewLambda(lambda_fun_name, capturing_names);
        loc;
      } in
      auto_release_expr env ~prepend_stmts ~append_stmts tmp
    )

    | If if_desc ->
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let tmp_var = "t[" ^ (Int.to_string tmp_id) ^ "]" in
      let spec = transform_expression_if env ~ret:tmp_var ~prepend_stmts ~append_stmts loc if_desc in
      let tmp_stmt = { C_op.Stmt.
        spec = If spec;
        loc;
      } in

      prepend_stmts := List.append !prepend_stmts [tmp_stmt];

      C_op.Expr.Temp tmp_id

    | Array arr_list -> (
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let arr_len = List.length arr_list in

      let tmp_sym = C_op.SymLocal ("t[" ^ (Int.to_string tmp_id) ^ "]") in

      let init_stmt = {
        C_op.Stmt.
        spec = Expr {
          C_op.Expr.
          spec = Assign(tmp_sym, {
            C_op.Expr.
            spec = NewArray arr_len;
            loc = Loc.none;
          });
          loc = Loc.none;
        };
        loc = Loc.none;
      } in

      let inits_exprs = List.map ~f:(transform_expression env) arr_list in

      let init_prepends = List.map ~f:(fun e -> e.prepend_stmts) inits_exprs in
      let init_appends = List.map ~f:(fun e -> e.append_stmts) inits_exprs in

      let init_stmts =
        List.mapi
        ~f:(fun index expr ->
          { C_op.Stmt.
            spec = Expr { C_op.Expr.
              spec = ArraySetValue(tmp_sym, index, expr.expr);
              loc = Loc.none;
            };
            loc = Loc.none;
          }
        )
        inits_exprs
      in

      prepend_stmts := List.concat [
        !prepend_stmts;
        List.concat init_prepends;
        [init_stmt];
        init_stmts;
      ];

      append_stmts := List.concat [
        if not is_move then [ gen_release_temp tmp_id ] else [];
        !append_stmts;
        List.concat init_appends;
      ];

      C_op.Expr.Temp tmp_id
    )

    | Call call -> (
      let open Expression in
      (* let current_scope = env.scope in *)
      let { callee; call_params; _ } = call in
      let params_struct = List.map ~f:(transform_expression env) call_params in

      let prepend, params, append = List.map ~f:(fun expr -> expr.prepend_stmts, expr.expr, expr.append_stmts) params_struct |> List.unzip3 in

      prepend_stmts := List.append !prepend_stmts (List.concat prepend);
      append_stmts := List.append !append_stmts (List.concat append);

      match callee with
      | { spec = Identifier (_, id); _ } -> (
        match Type_context.find_external_symbol env.ctx id with

        | Some ext_name -> (

          (* external method *)
          C_op.Expr.ExternalCall((C_op.SymLocal ext_name), None, params)
        )

        (* it's a local function *)
        | None -> (
          let deref_type = Type_context.deref_node_type env.ctx callee.ty_var in
          match deref_type with
          | Core_type.TypeExpr.Lambda _ -> (
            let transformed_callee = transform_expression env callee in
            prepend_stmts := List.append !prepend_stmts (List.append !prepend_stmts transformed_callee.prepend_stmts);
            append_stmts := List.append !append_stmts (List.append !append_stmts transformed_callee.append_stmts);
            C_op.Expr.CallLambda(transformed_callee.expr, params)
          )

          (* it's a contructor *)
          | _ ->
            let node = Type_context.get_node env.ctx id in
            let ctor_opt = Check_helper.find_typedef_of env.ctx node.value in
            let ctor_name, _ctor_ty_id = Option.value_exn ctor_opt in
            let name = find_variable env ctor_name.name in
            C_op.Expr.ExternalCall(name, None, params)
        )

      )

      | { spec = Member(expr, id); _ } -> (
        let expr_type = Type_context.deref_node_type env.ctx expr.ty_var in
        let member = Check_helper.find_member_of_type env.ctx ~scope:(Option.value_exn env.scope.raw) expr_type id.pident_name in
        match member with
        | Some (TypeDef ({ spec = ClassMethod { method_is_virtual = true; _ }; _ }, _), _) -> (
          let this_expr = transform_expression env expr in

          prepend_stmts := List.append !prepend_stmts this_expr.prepend_stmts;
          append_stmts := List.append !append_stmts this_expr.append_stmts;

          C_op.Expr.Invoke(this_expr.expr, id.pident_name, params);
        )

        | Some (TypeDef ({ spec = ClassMethod { method_get_set = None; _ }; _ }, method_id), _) -> (
          (* only class method needs a this_expr, this is useless for a static function *)
          let this_expr = transform_expression env expr in

          prepend_stmts := List.append !prepend_stmts this_expr.prepend_stmts;
          append_stmts := List.append !append_stmts this_expr.append_stmts;

          match Type_context.find_external_symbol env.ctx method_id with
          | Some ext_name -> (
            (* external method *)
            C_op.Expr.ExternalCall((C_op.SymLocal ext_name), Some this_expr.expr, params)
          )
          (* TODO: check if it's a virtual function *)
          | _ ->
            let callee_node = Type_context.get_node env.ctx callee.ty_var in
            let ctor_opt = Check_helper.find_typedef_of env.ctx callee_node.value in
            let _ctor_name, ctor_ty_id = Option.value_exn ctor_opt in
            let global_name = Hashtbl.find_exn env.global_name_map ctor_ty_id in
            C_op.Expr.ExternalCall(global_name, None, params)
        )

        (* it's a static function *)
        | Some (TypeDef ({ spec = Function _; _ }, fun_id), _) -> (
          let callee_node = Type_context.get_node env.ctx fun_id in
          let ctor_opt = Check_helper.find_typedef_of env.ctx callee_node.value in
          let _ctor_name, ctor_ty_id = Option.value_exn ctor_opt in
          let global_name = Hashtbl.find_exn env.global_name_map ctor_ty_id in
          C_op.Expr.ExternalCall(global_name, None, params)
        )

        | _ -> failwith (Format.sprintf "2 member %s is not callable" id.pident_name)
      )

      | _ -> (
        let callee_node = Type_context.get_node env.ctx callee.ty_var in
        let ctor_opt = Check_helper.find_typedef_of env.ctx callee_node.value in
        let _ctor_name, ctor_ty_id = Option.value_exn ctor_opt in
        let global_name = Hashtbl.find_exn env.global_name_map ctor_ty_id in
        C_op.Expr.ExternalCall(global_name, None, params)
      )
    )

    | Member(expr, id) -> (
      let expr_result = transform_expression env expr in
      prepend_stmts := List.append !prepend_stmts expr_result.prepend_stmts;
      append_stmts := List.append expr_result.append_stmts !append_stmts;

      let node = Type_context.get_node env.ctx expr.ty_var in

      let member = Check_helper.find_member_of_type env.ctx ~scope:(Option.value_exn env.scope.raw) node.value id.pident_name in
      (* could be a property of a getter *)
      let open Core_type.TypeDef in
      match member with
      | Some (Core_type.TypeExpr.TypeDef ({ spec = ClassMethod { method_get_set = Some Getter; _ }; _ }, method_id), _) -> (
        let ext_sym_opt = Type_context.find_external_symbol env.ctx method_id in
        let ext_sym = Option.value_exn ext_sym_opt in
        C_op.Expr.ExternalCall(SymLocal ext_sym, Some expr_result.expr, [])
      )

      (* it's a property *) 
      | Some _ -> (
        let expr_type = Type_context.deref_node_type env.ctx expr.ty_var in
        let expr_ctor_opt = Check_helper.find_construct_of env.ctx expr_type in
        match expr_ctor_opt with
        | Some (_, expr_id) -> (
          let cls_meta = Hashtbl.find_exn env.cls_meta_map expr_id in
          let prop_name = Hashtbl.find_exn cls_meta.cls_fields_map id.pident_name in
          C_op.Expr.GetField(expr_result.expr, cls_meta.cls_gen_name, prop_name)
        )

        | _ -> failwith "can not find ctor of expr, maybe it's a property"
      )

      | _ -> failwith (Format.sprintf "unexpected: can not find member %s of id %d" id.pident_name expr.ty_var)
    )

    | Unary _ -> failwith "n"

    | Binary (op, left, right) -> (
      let left' = transform_expression env left in
      let right' = transform_expression env right in

      prepend_stmts := List.concat [!prepend_stmts; left'.prepend_stmts; right'.prepend_stmts];
      append_stmts := List.concat [!append_stmts; left'.append_stmts; right'.append_stmts];

      let left_type = Type_context.deref_node_type env.ctx left.ty_var in

      let open Core_type in
      match (left_type, op) with
      | (TypeExpr.String, BinaryOp.Plus) ->
        let spec = auto_release_expr env ~prepend_stmts ~append_stmts {
          C_op.Expr.
          spec = ExternalCall(SymLocal "lc_std_string_concat", None, [left'.expr; right'.expr]);
          loc = Loc.none;
        } in
        spec

      | (TypeExpr.String, BinaryOp.Equal)
      | (TypeExpr.String, BinaryOp.NotEqual)
      | (TypeExpr.String, BinaryOp.LessThan)
      | (TypeExpr.String, BinaryOp.LessThanEqual)
      | (TypeExpr.String, BinaryOp.GreaterThan)
      | (TypeExpr.String, BinaryOp.GreaterThanEqual)
        ->
        let spec = auto_release_expr env ~prepend_stmts ~append_stmts {
          C_op.Expr.
          spec = StringCmp(op, left'.expr, right'.expr);
          loc = Loc.none;
        } in
        spec

      | _ ->
        C_op.Expr.I32Binary(op, left'.expr, right'.expr)
    )

    | Update _ -> failwith "update"

    (*
     * There are a lot of cases of assignment.
     *
     * 1. Assign to an identifier: a = <expr>
     *   - local variable
     *   - local RefCel
     *   - captured const
     *   - captured refcell
     * 2. Assign to a member: <expr>.xxx = <expr>
     *   - property of a class
     *   - setter of a class
     *   - Assign to this property: this.xxx = expr
     *)
    | Assign (op_opt, (name, _), expr) -> (
      let name = find_variable env name in
      let expr' = transform_expression env expr in

      prepend_stmts := List.append !prepend_stmts expr'.prepend_stmts;
      append_stmts := List.append !append_stmts expr'.append_stmts;

      match op_opt with
      | None ->
        C_op.Expr.Assign(name, expr'.expr)

      | Some op ->
        C_op.Expr.Update(op, name, expr'.expr)

    )

    | Init { init_name; init_elements; _ } -> (
      let init_name, init_name_id = init_name in

      let cls_meta = Hashtbl.find_exn env.cls_meta_map init_name_id in

      (* logical name -> real name *)
      let init_fiels_tuples = Hashtbl.to_alist cls_meta.cls_fields_map in

      let logical_names =
        List.map
          ~f:(fun real_name ->
            let logical_name, _ = List.find_exn ~f:(fun (_, real_name') -> String.equal real_name real_name') init_fiels_tuples in
            logical_name
          )
          cls_meta.cls_fields
      in

      let init_params =
        List.map
        ~f:(fun name ->
          let elm =
            List.find_exn
            ~f:(fun elm ->
              match elm with
              | InitEntry { init_entry_key; _} ->
                String.equal name init_entry_key.pident_name
              | _ -> false
            )
            init_elements
          in
          match elm with
          | InitEntry { init_entry_value; _} -> init_entry_value
          | _ -> failwith "unreachable"
        )
        logical_names
      in

      let params =
        List.map
        ~f:(fun p ->
          let tmp = transform_expression env p in
          prepend_stmts := List.append !prepend_stmts tmp.prepend_stmts;
          append_stmts := List.append tmp.append_stmts !append_stmts;
          tmp.expr
        )
        init_params
      in

      let fun_name = find_variable env init_name in
      C_op.Expr.ExternalCall((C_op.map_symbol ~f:(fun fun_name -> fun_name ^ "_init") fun_name), None, params)
    )

    | Block block ->
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let tmp_var = "t[" ^ (Int.to_string tmp_id) ^ "]" in

      let init = { C_op.Stmt.
        spec = Expr {
          C_op.Expr.
          spec = Assign (C_op.SymLocal tmp_var, {
            spec = Null;
            loc = Loc.none;
          });
          loc = Loc.none;
        };
        loc = Loc.none;
      } in

      let stmts = transform_block ~ret:tmp_var env block in

      prepend_stmts := List.concat [!prepend_stmts; [init]; stmts];

      C_op.Expr.Temp tmp_id

    | Match _match -> (
      let { match_expr; match_clauses; _ } = _match in
      let match_expr = transform_expression env match_expr in
      let result_tmp = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;

      prepend_stmts := List.append !prepend_stmts
        [{
          C_op.Stmt.
          spec = Expr {
            C_op.Expr.
            spec = Assign (C_op.SymLocal ("t[" ^ (Int.to_string result_tmp) ^ "]"), {
              spec = Null;
              loc = Loc.none;
            });
            loc = Loc.none;
          };
          loc = Loc.none;
        }];

      let tmp_counter = ref [] in

      List.iter
        ~f:(fun clause ->
          let saved_tmp_count = env.tmp_vars_count in
          let test_expr = transform_pattern_to_test env match_expr.expr clause.clause_pat in

          let body = transform_expression env clause.clause_consequent in

          prepend_stmts := List.append
            !prepend_stmts
            [{
              C_op.Stmt.
              spec = If {
                if_test = test_expr;
                if_consequent =
                  List.concat [
                    body.prepend_stmts;
                    [{ C_op.Stmt.
                      spec = Expr {
                        spec = Assign(C_op.SymLocal ("t[" ^ (Int.to_string result_tmp) ^ "]"), body.expr);
                        loc = Loc.none;
                      };
                      loc = Loc.none;
                    }];
                    body.append_stmts;
                  ];
                if_alternate = None;
              };
              loc = Loc.none;
            }];

          tmp_counter := env.tmp_vars_count::(!tmp_counter);
          env.tmp_vars_count <- saved_tmp_count;
        )
        match_clauses;

      (* use the max tmp vars *)
      env.tmp_vars_count <- List.fold ~init:0 ~f:(fun acc item -> if item > acc then item else acc) !tmp_counter;

      C_op.Expr.Temp result_tmp
    )

    | This ->
      C_op.Expr.Ident SymThis

    | Super -> failwith "not implemented: super"

    | Index(expr, index) -> (
      let expr = transform_expression env expr in
      let index = transform_expression env index in

      prepend_stmts := List.concat [ !prepend_stmts; index.prepend_stmts; expr.prepend_stmts ];
      append_stmts := List.concat [ expr.append_stmts; index.append_stmts; !append_stmts ];

      C_op.Expr.ArrayGetValue(expr.expr, { C_op.Expr. spec = IntValue index.expr; loc = Loc.none})
    )

  in
  {
    prepend_stmts = !prepend_stmts;
    expr = {
      spec = expr_spec;
      loc;
    };
    append_stmts = !append_stmts;
  }

and transform_expression_if env ?ret ~prepend_stmts ~append_stmts loc if_desc =
  let open Typedtree.Expression in

  let test = transform_expression env if_desc.if_test in

  let if_test = { C_op.Expr. spec = IntValue test.expr; loc = Loc.none; } in
  let consequent = transform_block ?ret env if_desc.if_consequent in
  let if_alternate =
    Option.map
    ~f:(fun alt ->
      match alt with
      | If_alt_if if_spec ->
        C_op.Stmt.If_alt_if (transform_expression_if env ?ret ~prepend_stmts ~append_stmts loc if_spec)
      | If_alt_block blk ->
        let consequent = transform_block ?ret env blk in
        C_op.Stmt.If_alt_block consequent
    )
    if_desc.if_alternative
  in
  
  let spec = {
    C_op.Stmt.
    if_test;
    if_consequent = consequent;
    if_alternate;
  } in

  prepend_stmts := List.concat [!prepend_stmts; test.prepend_stmts];
  append_stmts := List.append test.append_stmts !append_stmts;
  spec

and transform_block env ?ret (block: Typedtree.Block.t): C_op.Stmt.t list =
  let block_scope = create_scope_and_distribute_vars env block.scope in
  with_scope env block_scope (fun env ->
    let stmts = List.map ~f:(transform_statement ?ret env) block.body |> List.concat in

    let local_vars = block.scope#vars in
    let cleanup =
      if (List.length local_vars) > 0 then (
        List.map
          ~f:(fun (name, _) ->
            let sym = TScope.find_variable block_scope name in
            { C_op.Stmt.
              spec = Release {
                spec = Ident sym;
                loc = Loc.none;
              };
              loc = Loc.none;
            }
          )
          local_vars
      ) else [] in

    List.append stmts cleanup
  )

and transform_pattern_to_test env match_expr pat =
  let open Pattern in
  let { spec; loc } = pat in
  match spec with
  | Symbol (name, name_id) -> (
    let name_node = Type_context.get_node env.ctx name_id in
    let ctor_opt = Check_helper.find_typedef_of env.ctx name_node.value in
    let ctor, _ = Option.value_exn ~message:(Format.sprintf "find enum ctor failed: %s %d" name name_id) ctor_opt in
    let enum_ctor = Core_type.TypeDef.(
      match ctor.spec with
      | EnumCtor v -> v
      | _ -> failwith "n"
    ) in
    { C_op.Expr.
      spec = TagEqual(match_expr, enum_ctor.enum_ctor_tag_id);
      loc;
    }
  )
  | EnumCtor ((_name, name_id), _child) -> (
    let name_node = Type_context.get_node env.ctx name_id in
    let ctor_opt = Check_helper.find_typedef_of env.ctx name_node.value in
    let ctor, _ = Option.value_exn ctor_opt in
    let enum_ctor = Core_type.TypeDef.(
      match ctor.spec with
      | EnumCtor v -> v
      | _ -> failwith "n"
    ) in
    { C_op.Expr.
      spec = TagEqual(match_expr, enum_ctor.enum_ctor_tag_id);
      loc;
    }
  )

and generate_cls_meta cls gen_name =
  let open Declaration in
  let { cls_id; cls_body; _ } = cls in
  let properties =
    List.filter_map
      ~f:(fun elm ->
        match elm with
        | Cls_method _ -> None
        | Cls_property prop -> Some prop
        | Cls_declare _ -> None
      )
      cls_body.cls_body_elements;
  in
  let prop_names =
    List.map
    ~f:(fun prop -> prop.cls_property_name.pident_name)
    properties
  in
  let _, cls_name_id = cls_id in
  let result = create_cls_meta cls_name_id gen_name prop_names in
  List.iter
    ~f:(fun field_name -> Hashtbl.set result.cls_fields_map ~key:field_name ~data:field_name)
    result.cls_fields;
  result

and transform_class env cls: C_op.Decl.spec list =
  let open Declaration in
  let { cls_id; cls_body; _ } = cls in
  let original_name, cls_name_id = cls_id in
  let fun_name = distribute_name env original_name in
  let finalizer_name = fun_name ^ "_finalizer" in

  env.current_fun_name <- Some (create_current_fun_meta original_name);

  Hashtbl.set env.global_name_map ~key:cls_name_id ~data:(SymLocal fun_name);

  let cls_meta = generate_cls_meta cls fun_name in
  Hashtbl.set env.cls_meta_map ~key:cls_meta.cls_id ~data:cls_meta;

  let class_methods = ref [] in

  let methods: C_op.Decl.spec list = 
    List.fold
      ~init:[]
      ~f:(fun acc elm ->
        match elm with
        | Cls_method _method -> (
          let { cls_method_name; cls_method_params; cls_method_body; cls_method_scope; _ } = _method in
          (* let fun_name = distribute_name env cls_method_name in *)
          let origin_method_name, method_id = cls_method_name in
          let new_name = original_name ^ "_" ^ origin_method_name in
          let new_name = distribute_name env new_name in

          Hashtbl.set env.global_name_map ~key:method_id ~data:(SymLocal new_name);

          let open Core_type in
          let node_type = Type_context.deref_node_type env.ctx method_id in
          let is_virtual =
            match node_type with
            | TypeExpr.TypeDef ({ spec = TypeDef.ClassMethod { method_is_virtual = true; _ }; _ }, _) ->
              true
            | _ -> false
          in

          if is_virtual then (
            class_methods := { C_op.Decl.
              class_method_name = origin_method_name;
              class_method_gen_name = new_name;
            }::!class_methods
          );

          let _fun =
            transform_function_impl
              env
              ~name:new_name
              ~params:cls_method_params
              ~scope:(Option.value_exn cls_method_scope)
              ~body:cls_method_body
              ~comments:[]
          in

          _fun::acc
          (* { C_op.Decl. spec = _fun; loc = cls_method_loc }::acc *)
        )
        | Cls_property _ -> acc
        | Cls_declare _ -> failwith "unimplemented: declare"
      )
      cls_body.cls_body_elements;
  in

  env.class_inits <- { C_op.Decl.
    class_name = fun_name;
    class_id_name = fun_name ^ "_class_id";
    class_def_name = fun_name ^ "_def";
    class_methods = List.rev !class_methods;
  }::env.class_inits;

  let finalizer = generate_finalizer finalizer_name cls_meta in

  let cls = {
    C_op.Decl.
    name = fun_name;
    original_name;
    finalizer = Some finalizer;
    properties = cls_meta.cls_fields;
  } in

  List.append [ (C_op.Decl.Class cls) ] methods

and generate_finalizer name class_meta =
  let this_expr = {
    C_op.Expr.
    spec = Ident SymThis;
    loc = Loc.none;
  } in
  let finalizer_content =
    List.map
    ~f:(fun field_name -> {
      C_op.Stmt.
      spec = Release { C_op.Expr.
        spec = GetField (this_expr, class_meta.cls_gen_name, field_name);
        loc = Loc.none;
      };
      loc = Loc.none;
    })
    class_meta.cls_fields
  in
  {
    finalizer_name = name;
    finalizer_content;
  }

and transform_enum env enum =
  let open Enum in
  let { cases; _ } = enum in
  List.mapi
    ~f:(fun index case ->
      let case_name, _ = case.case_name in
      let new_name = distribute_name env case_name in
      C_op.Decl.EnumCtor {
        enum_ctor_name = new_name;
        enum_ctor_tag_id = index;
        enum_cotr_params_size = List.length case.case_fields;
      }
    )
    cases

and transform_lambda env ~lambda_gen_name content _ty_var =
  let fake_block = {
    Block.
    body = [
      { Statement.
        spec = Expr content.lambda_body;
        loc = Loc.none;
        attributes = [];
      }
    ];
    scope = content.lambda_scope;
    loc = Loc.none;
    return_ty = content.lambda_body.ty_var;
  } in

  let _fun = transform_function_impl env
    ~name:lambda_gen_name
    ~params:content.lambda_params
    ~scope:content.lambda_scope
    ~body:fake_block
    ~comments:[]
  in

  {
    C_op.Decl.
    spec = _fun;
    loc = Loc.none;
  }

type result = {
  main_function_name: string option;
  declarations: C_op.Decl.t list;
  global_class_init: string option;
}

let transform_declarations ctx declarations =
  let env = create ctx in

  let declarations =
    List.map ~f:(transform_declaration env) declarations
    |> List.concat
  in

  let declarations, global_class_init =
    if List.is_empty env.class_inits then
      declarations, None
    else (
      let name = "LC_class_init" in
      (List.append declarations [{
        C_op.Decl.
        spec = GlobalClassInit(name, List.rev env.class_inits);
        loc = Loc.none;
      }]), Some name
    )
  in

  {
    main_function_name = env.main_function_name;
    declarations;
    global_class_init;
  }
