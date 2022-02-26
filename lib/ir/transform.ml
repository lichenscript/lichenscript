(*
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)
open Core_kernel
open Lichenscript_parsing.Asttypes
open Lichenscript_parsing.Ast
open Lichenscript_typing
open Lichenscript_typing.Scope
open Lichenscript_typing.Typedtree
(*
 * Convert TypedTree to C-op
 *)

module TScope = struct
  (* Why name_map?
   * 1. The same variable(type_id) has different meaning in different scope,
   *    In a lambda expression, a captured value represents a value to this,
   *    but int outer scope, it's a local variable.
   *)
  type t = {
    name_map: (string, Ir.symbol) Hashtbl.t;

    local_vars_to_release: Ir.symbol list ref;

    raw: scope option;
    prev: t option;
  }

  let create scope =
    let name_map = Hashtbl.create (module String) in
    {
      name_map;
      local_vars_to_release = ref [];
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

  let add_vars_to_release scope (name: Ir.symbol) =
    scope.local_vars_to_release := name::!(scope.local_vars_to_release)

  let[@warning "-unused-value-declaration"] remove_release_var scope name =
    scope.local_vars_to_release :=
      List.filter
        ~f:(fun sym ->
          match (sym, name) with
          | (Ir.SymLocal local_name, Ir.SymLocal name) ->
            not (String.equal local_name name)
          | _ -> true
        )
        !(scope.local_vars_to_release)

  let rec get_symbols_to_release_til_function acc scope =
    let acc = List.append acc !(scope.local_vars_to_release) in
    let raw = Option.value_exn scope.raw in
    if raw#test_function_scope then
      acc
    else (
      let prev_scope = Option.value_exn scope.prev in
      get_symbols_to_release_til_function acc prev_scope
    )

  let rec get_symbols_to_release_til_while acc scope =
    let acc = List.append acc !(scope.local_vars_to_release) in
    let raw = Option.value_exn scope.raw in
    if raw#test_while_scope then
      acc
    else (
      let prev_scope = Option.value_exn scope.prev in
      get_symbols_to_release_til_while acc prev_scope
    )

  let rec is_in_class scope =
    match scope.raw with
    | Some raw -> (
      if Option.is_some raw#test_class_scope then
        true
      else (
        match scope.prev with
        | Some prev -> is_in_class prev
        | None -> false
      )
    )
    | None -> false

  let is_in_lambda scope =
    match scope.raw with
    | Some raw -> (
      if raw#test_lambda_scope then
        true
      else (
        match scope.prev with
        | Some prev -> is_in_class prev
        | None -> false
      )
    )
    | None -> false
  
end

(*
 * It's a monad, represent the `continuation` of pattern test
 * if (test) {
 *   (* continuation here *)
 * }
 *)
module PMMeta = struct

  type generateor = (finalizers:Ir.Stmt.t list -> unit -> Ir.Stmt.t list)
  type t = (generateor -> Ir.Stmt.t list)

  let (>>=) (left: t) (right: t) = 
    (fun generateor ->
      left (fun ~finalizers:left_finalizers () ->
        right (fun ~finalizers:right_finalizers () ->
          let merged_finalizers = List.append left_finalizers right_finalizers in
          generateor ~finalizers:merged_finalizers ()
        )
      ) 
    )
  
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
  cls_fields: (string * int) list;
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
  mutable class_inits: Ir.Decl.class_init list;

  mutable prepends_decls: Ir.Decl.t list;

  (*
   * Some variables are local, but some are global,
   * such as a method of a class, the contructor of enum, etc
   *)
  global_name_map: (int, Ir.symbol) Hashtbl.t;

  cls_meta_map: (int, cls_meta) Hashtbl.t;

  (* for lambda generation *)
  mutable current_fun_meta: current_fun_meta option;
  mutable lambdas: Ir.Decl.t list;
}

let[@warning "-unused-value-declaration"] is_identifier expr =
  let open Typedtree.Expression in
  match expr.spec with
  | Typedtree.Expression.Identifier _ -> true
  | _ -> false

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
  prepend_stmts: Ir.Stmt.t list;
  expr: Ir.Expr.t;
  append_stmts: Ir.Stmt.t list;
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
    prepends_decls = [];
    global_name_map;
    cls_meta_map;
    current_fun_meta = None;
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
  let { spec; loc; attributes } = decl in
  match spec with
  | Class cls -> (
    transform_class env cls loc
  )
  | Function_ _fun -> (
    let specs = transform_function env _fun in
    let lambdas = env.lambdas in
    env.lambdas <- [];
    let bodys = List.map ~f:(fun spec -> { Ir.Decl. spec; loc }) specs in
    List.append lambdas bodys
  )

  | Enum enum -> transform_enum env enum loc

  | Interface _ -> []

  | Declare declare -> (
    if List.is_empty attributes then (
      match declare.decl_spec with
      | DeclFunction fun_header -> (
        let phys_name, _ = fun_header.name in
        let name = find_or_distribute_name env declare.decl_ty_var phys_name in
        let decl = { Ir.Decl.
          spec = FuncDecl name;
          loc = Loc.none;
        } in
        [decl]
      )
    ) else
      []
  )

  | Import _ -> []

and distribute_name env =
  TScope.distribute_name env.scope

and find_or_distribute_name env id name =
  match Hashtbl.find env.global_name_map id with
  | Some name -> name
  | None -> (
    let new_name = Ir.SymLocal (distribute_name env name) in
    Hashtbl.set env.global_name_map ~key:id ~data:new_name;
    new_name
  )

(*
 * 1. Scan the function firstly, find out all the lambda expression
 *    and captured variables
 * 2. Transform body
 *)
and transform_function env _fun =
  let open Function in
  let { body; comments; header; scope; _ } = _fun in
  let original_name, original_name_id = header.name in

  env.current_fun_meta <- Some (create_current_fun_meta original_name);

  let fun_name = 
    match find_or_distribute_name env original_name_id original_name with
    | Ir.SymLocal l -> l
    | _ -> failwith "unreachable"
  in

  if String.equal original_name "main" then (
    env.main_function_name <- Some fun_name
  );

  let result = transform_function_impl env ~name:fun_name ~params:header.params ~scope ~body ~comments in

  env.current_fun_meta <- None;

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

  let fun_meta = Option.value_exn env.current_fun_meta in
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

  let max_tmp_value = ref 0 in

  (* let before_stmts = env.tmp_vars_count in *)

  let stmts =
    List.map
      ~f:(fun stmt ->
        env.tmp_vars_count <- 0;

        let result = transform_statement env stmt in

        if env.tmp_vars_count > !max_tmp_value then (
          max_tmp_value := env.tmp_vars_count
        );

        result
      )
      body.body
    |> List.concat
  in

  let generate_name_def () =
    let names = fun_meta.def_local_names in
    [{ Ir.Stmt.
      spec = VarDecl names;
      loc = Loc.none;
    }]
  in

  let ending_parts =
    match List.last body.body with
    | Some { Typedtree.Statement. spec = Return _; _ } -> []
    | Some { Typedtree.Statement. spec = Expr _; _ } ->
      let cleanup = generate_finalize_stmts fun_scope in

      let return_stmt = { Ir.Stmt.
        spec = Return (Some (Ir.Expr.Ident (Ir.SymLocal "ret")));
        loc = Loc.none;
      } in
      List.append cleanup [return_stmt]

    | _ -> (
      let cleanup = generate_finalize_stmts fun_scope in

      let return_stmt = { Ir.Stmt.
        spec = Return (Some Ir.Expr.Null);
        loc = Loc.none;
      } in
      List.append cleanup [return_stmt]
    )
  in

  let def =
    if (List.length fun_meta.def_local_names) > 0 then (
      generate_name_def ()
    ) else
      []
  in

  let new_body = {
    Ir.Block.
    body = List.concat [ def; stmts; ending_parts ];
    loc = body.loc;
  } in

  let t_fun = {
    Ir.Func.
    name;
    body = new_body;
    tmp_vars_count = !max_tmp_value;
    comments;
  } in

  pop_scope env;

  env.tmp_vars_count <- !max_tmp_value;

  Ir.Decl.Func t_fun;

and create_scope_and_distribute_vars env raw_scope =
  let scope = TScope.create (Some raw_scope) in
  let local_vars = raw_scope#vars in

  distribute_name_to_scope scope (Option.value_exn env.current_fun_meta) local_vars;

  scope

and transform_statement ?ret env stmt =
  let open Statement in
  let { spec; loc; _ } = stmt in
  let transform_return_expr ?ret expr =
    let tmp = transform_expression ~is_move:true env expr in
    let ret = Option.value ~default:(Ir.SymRet) ret in
    let assign = Ir.Expr.Assign(Ident ret, tmp.expr) in
    (*
     * It's returning the function directly, it's not need to retain,
     * because it's directly assining to "ret" variable.
     *
     * But it should retain if it's a normal block.
     *)
    let expr = {
      Ir.Stmt.
      spec = Expr assign;
      loc;
    } in
    List.concat [ tmp.prepend_stmts; [ expr ]; tmp.append_stmts ]
  in
  match spec with
  | Expr expr -> transform_return_expr ?ret expr

  | Semi expr -> (
    let tmp = transform_expression env expr in
    let expr = {
      Ir.Stmt.
      spec = Expr tmp.expr;
      loc;
    } in
    List.concat [ tmp.prepend_stmts; [ expr ]; tmp.append_stmts ]
  )

  | While { while_test; while_block; while_loc } -> (
    let while_test' = transform_expression env while_test in

    let bodys = transform_block ?ret env while_block in
    let body = {
      Ir.Block.
      body =
        List.append while_test'.append_stmts bodys;
      loc = while_block.loc;
    } in

    List.concat [
      while_test'.prepend_stmts;
      [
        { Ir.Stmt.
          spec = While(while_test'.expr, body);
          loc = while_loc;
        }
      ];
      while_test'.append_stmts;
    ]
  )

  | Binding binding -> (
    let original_name, name_id =
      match binding.binding_pat with
      | { spec = Pattern.Symbol sym; _ } -> sym
      | _ -> failwith "unrechable"
    in

    let scope = env.scope in

    let variable = Option.value_exn
      ~message:(Format.sprintf "%d:%d can not find %s" binding.binding_loc.start.line binding.binding_loc.start.column original_name)
      ((Option.value_exn scope.raw)#find_var_symbol original_name)
    in

    let name = TScope.find_variable scope original_name in
    let init_expr = transform_expression ~is_move:true env binding.binding_init in

    let node_type = Type_context.deref_node_type env.ctx name_id in
    let need_release = not (Check_helper.type_should_not_release env.ctx node_type) in
    (*
     * if a variable is captured, it will be upgraded into a RefCell.
     * Whatevet type it is, it should be released.
     *)
    if need_release || !(variable.var_captured) then (
      TScope.add_vars_to_release scope name
    );

    let assign_expr =
      if should_var_captured variable then (
        let init_expr = Ir.Expr.NewRef init_expr.expr in
        Ir.Expr.Assign((Ident name), init_expr)
      ) else
        Ir.Expr.Assign((Ident name), init_expr.expr);
    in

    List.concat [
      init_expr.prepend_stmts;
      [
        { Ir.Stmt.
          spec = Expr assign_expr;
          loc = binding.binding_loc;
        }
      ];
      init_expr.append_stmts;
    ]
  )

  | Break _ ->
    let stmts = generate_finalize_stmts_while env.scope in
    List.append
      stmts
      [
        { Ir.Stmt.
          spec = Break;
          loc;
        }
      ]

  | Continue _ ->
    let stmts = generate_finalize_stmts_while env.scope in
    List.append
      stmts
      [
        { Ir.Stmt.
          spec = Continue;
          loc;
        }
      ]

  | Debugger -> []

  | Return ret_opt -> (
    let cleanup = generate_finalize_stmts_function env.scope in
    match ret_opt with
    | Some ret ->
      let ret = transform_return_expr ~ret:(Ir.SymRet) ret in
      let ret_stmt = { Ir.Stmt.
        spec = Return (Some (Ir.Expr.Ident Ir.SymRet));
        loc;
      } in
      List.concat [
        ret;
        cleanup;
        [ret_stmt];
      ]
    | None ->
      let ret_stmt = { Ir.Stmt.
        spec = Return (Some Ir.Expr.Null);
        loc;
      } in
      List.append cleanup [ret_stmt]
  )

  | Empty -> []

and gen_release_temp id =
  { Ir.Stmt.
    spec = Release (Ir.Expr.Temp id);
    loc = Loc.none;
  }

and auto_release_expr env ?(is_move=false) ~append_stmts ty_var expr =
  let node_type = Type_context.deref_node_type env.ctx ty_var in
  if is_move || Check_helper.type_should_not_release env.ctx node_type then (
    expr
  ) else (
    let tmp_id = env.tmp_vars_count in
    env.tmp_vars_count <- env.tmp_vars_count + 1;

    let assign_expr = Ir.Expr.Assign(
      (Ident (Ir.SymTemp tmp_id)), expr)
    in

    append_stmts := (gen_release_temp tmp_id)::!append_stmts;

    assign_expr
  )

and prepend_expr env ~prepend_stmts ~append_stmts (expr: expr_result) =
  let tmp_id = env.tmp_vars_count in
  env.tmp_vars_count <- env.tmp_vars_count + 1;

  let assign_expr = Ir.Expr.Assign(
    (Ident (Ir.SymTemp tmp_id)),
    expr.expr
  ) in

  let prepend_stmt = {
    Ir.Stmt.
    spec = Expr assign_expr;
    loc = Loc.none;
  } in

  prepend_stmts := List.append !prepend_stmts [prepend_stmt];

  let result = Ir.Expr.Temp tmp_id in

  append_stmts := (gen_release_temp tmp_id)::!append_stmts;

  result

and try_folding_literals op left right original_expr =
  let open Expression in

  let do_integer_folding int_op left_int right_int =
    let new_int =
      (* TODO: i64 support *)
      let open Int32 in
      match int_op with
      | BinaryOp.Minus -> left_int - right_int
      | BinaryOp.Plus -> left_int + right_int
      | BinaryOp.Div -> left_int / right_int
      | BinaryOp.Mult -> left_int * right_int
      | _ -> failwith "unsupported binary op between integer"
    in
    Constant(Literal.Integer(new_int))
  in

  let do_float_folding float_op left_str right_str =
    let new_float =
      let open Float in
      let left_f = Float.of_string left_str in
      let right_f = Float.of_string right_str in
      match float_op with
      | BinaryOp.Minus -> left_f - right_f
      | BinaryOp.Plus -> left_f + right_f
      | BinaryOp.Div -> left_f / right_f
      | BinaryOp.Mult -> left_f * right_f
      | _ -> failwith "unsupported binary op between float"
    in
    Constant(Literal.Float(Float.to_string new_float, None))
  in

  match (left.spec, right.spec) with
  | (Constant(Integer left_int), Constant(Integer right_int)) -> (
    match op with
    | BinaryOp.Plus
    | BinaryOp.Minus
    | BinaryOp.Div
    | BinaryOp.Mult -> (
      let new_spec = do_integer_folding op left_int right_int in
      Some { left with spec = new_spec }
    )
    | _ -> None
  )
  | (Constant(Float(left_f_str, _)), Constant(Float(right_f_str, _))) -> (
    match op with
    | BinaryOp.Plus
    | BinaryOp.Minus
    | BinaryOp.Div
    | BinaryOp.Mult -> (
      let new_spec = do_float_folding op left_f_str right_f_str in
      Some { left with spec = new_spec }
    )
    | _ -> None
  )
  | (Constant(String(left_str, _, _)), Constant(String(right_str, _, _))) -> (
    match op with 
    | BinaryOp.Plus -> (
      let concated = String.concat [left_str; right_str] in
      let new_spec = Constant(Literal.String(concated, left.loc, None)) in
      Some { left with spec = new_spec }
    )
    | _ -> None
  )
  | (Binary(_, _, _), _) -> (
    let open Option in
    let res = try_folding_binary_expression left in
    res >>= fun left' ->
    (* if left being folded, try rec folding left' with right *)
    try_folding_literals op left' right { original_expr with spec = Binary(op, left', right) }
  )
  | (_, Binary(_, _, _)) -> (
    let open Option in
    let res = try_folding_binary_expression right in
    res >>= fun right' ->
    (* if right being folded, try rec folding left with right' *)
    try_folding_literals op left right' { original_expr with spec = Binary(op, left, right) }
  )
  | _ -> None

and try_folding_binary_expression expr: Expression.t option =
  let open Expression in

  match expr.spec with
  | Binary (op, left, right) -> (
    (* here, we try fold binary expression to constant expression like: 
       input: Expression { spec: BinaryAdd(Constant(Literal.Integer(1)), Constant(Literal.Integer(2))) })
       output: Some(Expression { spec: Constant(Literal.Integer(3)) })
       
       if can't be folded, None will be returned *)
    try_folding_literals op left right expr
  )
  | _ -> None

and transform_expression ?(is_move=false) ?(is_borrow=false) env expr =
  let open Expression in
  let { spec; loc; ty_var; _ } = expr in
  let prepend_stmts = ref [] in
  let append_stmts = ref [] in
  let expr_spec =
    match spec with
    | Constant literal -> (
      let open Literal in
      match literal with
      | Unit ->
        Ir.Expr.Null

      | String(content, _, _) -> 
        auto_release_expr env ~is_move ~append_stmts ty_var (Ir.Expr.NewString content)

      | Integer content ->
        let int_str = Int32.to_string content in
        Ir.Expr.NewInt int_str

      | Boolean bl ->
        Ir.Expr.NewBoolean bl

      | Float (fl, _) ->
        Ir.Expr.NewFloat fl

      | Char ch ->
        Ir.Expr.NewChar ch

    )

    (*
     * 1. local variable
     * 2. enum constructor
     *)
    | Identifier (name, name_id) -> (
      let first_char = String.get name 0 in
      if Char.is_uppercase first_char then (
        let node_type = Type_context.deref_node_type env.ctx name_id in
        let ctor = Check_helper.find_construct_of env.ctx node_type in
        let open Core_type in
        match ctor with
        | Some({ TypeDef. id = ctor_id; spec = EnumCtor _; _ }, _) ->
          let generate_name = Hashtbl.find_exn env.global_name_map ctor_id in
          (* let sym = find_variable env name in *)
          Ir.Expr.ExternalCall(generate_name, None, [])

        | _ ->
          let id_ty = Type_context.print_type_value env.ctx node_type in
          failwith (Format.sprintf "unknown identifier: %s" id_ty)
      ) else (
        let variable_opt = (Option.value_exn env.scope.raw)#find_var_symbol name in
        let variable = Option.value_exn variable_opt in
        let sym = find_variable env name in
        let id_expr =
          if should_var_captured variable then
            Ir.Expr.GetRef sym
          else
            Ir.Expr.Ident sym
        in

        let node_type = Type_context.deref_node_type env.ctx variable.var_id in
        let need_release = not (Check_helper.type_should_not_release env.ctx node_type) in

        if is_move && need_release then (
          (* TScope.remove_release_var env.scope sym;
          id_expr *)
          let id_expr = id_expr in
          Ir.Expr.Retaining id_expr
        ) else if (not is_borrow) && (not is_move) && need_release then  (
          let id_expr = id_expr in
          Ir.Expr.Retaining id_expr
        ) else
          id_expr
      )
    )

    | Lambda lambda_content -> (
      let parent_scope = env.scope in
      let fun_meta = Option.value_exn ~message:"current function name not found" env.current_fun_meta in
      let lambda_name = fun_meta.fun_name ^ "_lambda_" ^ (Int.to_string ty_var) in
      let lambda_fun_name = "LCC_" ^ lambda_name in

      let capturing_variables = lambda_content.lambda_scope#capturing_variables in

      let capturing_names = Array.create ~len:(Scope.CapturingVarMap.length capturing_variables) (Ir.SymLocal "<unexpected>") in

      (* pass the capturing values into the deeper scope *)
      Scope.CapturingVarMap.iteri
      ~f:(fun ~key ~data -> 
        let name = TScope.find_variable parent_scope key in
        Array.set capturing_names data name
      )
      capturing_variables;

      let lambda = transform_lambda env ~lambda_name lambda_content ty_var in

      env.lambdas <- lambda::env.lambdas;

      let this_expr =
        if TScope.is_in_class env.scope then
          Ir.Expr.Ident(Ir.SymThis)
        else
          Null
      in

      let expr = Ir.Expr.NewLambda(lambda_fun_name, this_expr, capturing_names) in
      auto_release_expr env ~is_move ~append_stmts ty_var expr
    )

    | If if_desc ->
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let tmp_var = Ir.SymTemp tmp_id in

      if Check_helper.is_unit env.ctx (Type_context.deref_node_type env.ctx ty_var) then (
        let init_stmt = { Ir.Stmt.
          spec = Expr(Ir.Expr.Assign(Ident tmp_var, Null));
          loc = Loc.none;
        } in

        prepend_stmts := List.append !prepend_stmts [init_stmt];
      );

      let spec = transform_expression_if env ~ret:tmp_var ~prepend_stmts ~append_stmts loc if_desc in
      let tmp_stmt = { Ir.Stmt.
        spec = If spec;
        loc;
      } in

      prepend_stmts := List.append !prepend_stmts [tmp_stmt];

      Ir.Expr.Temp tmp_id

    | Tuple children ->
      let children_expr =
        List.map
          ~f:(fun expr ->
            let tmp = transform_expression env expr in

            prepend_stmts := List.append !prepend_stmts tmp.prepend_stmts;
            append_stmts := List.append tmp.append_stmts !append_stmts;

            tmp.expr
          )
          children
      in
      Ir.Expr.ExternalCall(Ir.SymLocal "LCNewTuple", None, children_expr)

    | Array arr_list -> (
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let arr_len = List.length arr_list in

      let tmp_sym = Ir.SymTemp tmp_id in

      let init_stmt = {
        Ir.Stmt.
        spec = Expr (
          Ir.Expr.Assign(
            (Ident tmp_sym),
            (Ir.Expr.NewArray arr_len)
          )
        );
        loc = Loc.none;
      } in

      let inits_exprs = List.map ~f:(transform_expression env) arr_list in

      let init_prepends = List.map ~f:(fun e -> e.prepend_stmts) inits_exprs in
      let init_appends = List.map ~f:(fun e -> e.append_stmts) inits_exprs in

      let init_stmts =
        List.mapi
        ~f:(fun index expr ->
          { Ir.Stmt.
            spec = Expr (
              Ir.Expr.ExternalCall(
                SymLocal "LCArraySetValue",
                Some (Ident tmp_sym),
                [
                  (NewInt (Int.to_string index));
                  expr.expr
                ]
              );
            );
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

      Ir.Expr.Temp tmp_id
    )

    | Map entries -> (
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;

      let init_size = List.length entries in

      let init_stmt = { Ir.Stmt.
        spec = Expr (Ir.Expr.Assign(Ir.Expr.Temp tmp_id, Ir.Expr.NewMap init_size));
        loc = expr.loc;
      } in

      let pre, set_stmts, app =
        entries
        |> List.map
          ~f:(fun entry ->
            let open Literal in
            let key_expr =
              match entry.map_entry_key with
              | String(content, _, _) -> 
                auto_release_expr env ~is_move:false ~append_stmts ty_var (Ir.Expr.NewString content)

              | Integer content ->
                let int_str = Int32.to_string content in
                Ir.Expr.NewInt int_str

              | _ ->
                failwith "unimplemented"
            in
            let expr = transform_expression env entry.map_entry_value in
            let set_expr = Ir.Expr.ExternalCall(
              Ir.SymLocal "lc_std_map_set",
              Some (Ir.Expr.Temp tmp_id),
              [key_expr; expr.expr]
              )
            in
            expr.prepend_stmts,
            { Ir.Stmt.
              spec = Expr set_expr;
              loc = entry.map_entry_loc;
            },
            expr.append_stmts
          )
        |> List.unzip3
      in

      prepend_stmts := List.concat [
        !prepend_stmts;
        List.concat pre;
        [init_stmt];
        set_stmts
      ];

      append_stmts := List.concat [
        List.concat app;
        !append_stmts;
      ];

      Ir.Expr.Temp tmp_id
    )

    | Call call -> (
      let open Expression in
      (* let current_scope = env.scope in *)
      let { callee; call_params; _ } = call in
      let params_struct = List.map ~f:(transform_expression ~is_borrow:true env) call_params in

      let prepend, params, append = List.map ~f:(fun expr -> expr.prepend_stmts, expr.expr, expr.append_stmts) params_struct |> List.unzip3 in

      prepend_stmts := List.append !prepend_stmts (List.concat prepend);
      append_stmts := List.append !append_stmts (List.concat append);

      let call_expr =
        match callee with
        | { spec = Identifier (_, id); _ } -> (
          match Type_context.find_external_symbol env.ctx id with

          | Some ext_name -> (

            (* external method *)
            Ir.Expr.ExternalCall((Ir.SymLocal ext_name), None, params)
          )

          (* it's a local function *)
          | None -> (
            let deref_type = Type_context.deref_node_type env.ctx callee.ty_var in
            match deref_type with
            | Core_type.TypeExpr.Lambda _ -> (
              let transformed_callee = transform_expression ~is_borrow:true env callee in
              prepend_stmts := List.append !prepend_stmts (List.append !prepend_stmts transformed_callee.prepend_stmts);
              append_stmts := List.append !append_stmts (List.append !append_stmts transformed_callee.append_stmts);
              Ir.Expr.CallLambda(transformed_callee.expr, params)
            )

            (* it's a contructor *)
            | _ ->
              let node = Type_context.get_node env.ctx id in
              let ctor_opt = Check_helper.find_typedef_of env.ctx node.value in
              let ctor_name = Option.value_exn ctor_opt in
              let name = find_variable env ctor_name.name in
              Ir.Expr.ExternalCall(name, None, params)
          )

        )

        | { spec = Member(expr, id); _ } ->
          begin
            let expr_type = Type_context.deref_node_type env.ctx expr.ty_var in
            let member = Check_helper.find_member_of_type env.ctx ~scope:(Option.value_exn env.scope.raw) expr_type id.pident_name in
            match member with
            | Some ((Method ({ spec = ClassMethod { method_is_virtual = true; _ }; _ }, _, _)), _) -> (
                let this_expr = transform_expression ~is_borrow:true env expr in

                prepend_stmts := List.append !prepend_stmts this_expr.prepend_stmts;
                append_stmts := List.append !append_stmts this_expr.append_stmts;

                Ir.Expr.Invoke(this_expr.expr, id.pident_name, params)
            )
            | Some ((Method ({ id = method_id; spec = ClassMethod { method_get_set = None; _ }; _ }, _, _)), _) -> (
              (* only class method needs a this_expr, this is useless for a static function *)
              let this_expr = transform_expression ~is_borrow:true env expr in

              prepend_stmts := List.append !prepend_stmts this_expr.prepend_stmts;
              append_stmts := List.append !append_stmts this_expr.append_stmts;

              match Type_context.find_external_symbol env.ctx method_id with
              | Some ext_name -> (
                (* external method *)
                Ir.Expr.ExternalCall((Ir.SymLocal ext_name), Some this_expr.expr, params)
              )
              | _ ->
                let callee_node = Type_context.get_node env.ctx method_id in
                let ctor_opt = Check_helper.find_typedef_of env.ctx callee_node.value in
                let ctor = Option.value_exn ~message:"Cannot find typedef of class" ctor_opt in
                let ctor_ty_id = ctor.id in
                let global_name = Hashtbl.find_exn env.global_name_map ctor_ty_id in
                Ir.Expr.ExternalCall(global_name, Some this_expr.expr, params)
            )

            (* it's a static function *)
            | Some (TypeDef { id = fun_id; spec = Function _; _ }, _) -> (
              let callee_node = Type_context.get_node env.ctx fun_id in
              let ctor_opt = Check_helper.find_typedef_of env.ctx callee_node.value in
              let ctor = Option.value_exn ctor_opt in
              let ctor_ty_id = ctor.id in
              let global_name = Hashtbl.find_exn env.global_name_map ctor_ty_id in
              Ir.Expr.ExternalCall(global_name, None, params)
            )
            | _ -> failwith "unrechable"

          end

        | _ -> (
          let callee_node = Type_context.get_node env.ctx callee.ty_var in
          let ctor_opt = Check_helper.find_typedef_of env.ctx callee_node.value in
          let ctor = Option.value_exn ctor_opt in
          let ctor_ty_id = ctor.id in
          let global_name = Hashtbl.find_exn env.global_name_map ctor_ty_id in
          Ir.Expr.ExternalCall(global_name, None, params)
        )
      in
      auto_release_expr ~is_move env ~append_stmts ty_var call_expr
    )

    | Member(main_expr, id) -> (
      let expr_result = transform_expression ~is_borrow:true env main_expr in
      prepend_stmts := List.append !prepend_stmts expr_result.prepend_stmts;
      append_stmts := List.append expr_result.append_stmts !append_stmts;

      let node = Type_context.get_node env.ctx main_expr.ty_var in

      let member = Check_helper.find_member_of_type env.ctx ~scope:(Option.value_exn env.scope.raw) node.value id.pident_name in
      (* could be a property of a getter *)
      let open Core_type.TypeDef in
      match member with
      | Some (Method ({ Core_type.TypeDef. id = def_int; spec = ClassMethod { method_get_set = Some _; _ }; _ }, _params, _rt), _) -> (
        let ext_sym_opt = Type_context.find_external_symbol env.ctx def_int in
        let ext_sym = Option.value_exn ext_sym_opt in
        Ir.Expr.ExternalCall(SymLocal ext_sym, Some expr_result.expr, [])
      )

      | Some (Core_type.TypeExpr.TypeDef { id = method_id; spec = ClassMethod { method_get_set = Some Getter; _ }; _ }, _) -> (
        let ext_sym_opt = Type_context.find_external_symbol env.ctx method_id in
        let ext_sym = Option.value_exn ext_sym_opt in
        Ir.Expr.ExternalCall(SymLocal ext_sym, Some expr_result.expr, [])
      )

      (* it's a property *) 
      | Some _ -> (
        let expr_type = Type_context.deref_type env.ctx node.value in
        let expr_ctor_opt = Check_helper.find_construct_of env.ctx expr_type in
        match expr_ctor_opt with
        | Some({ id = expr_id; _ }, _) -> (
          let cls_meta = Hashtbl.find_exn env.cls_meta_map expr_id in
          let prop_name = Hashtbl.find_exn cls_meta.cls_fields_map id.pident_name in
          Ir.Expr.GetField(expr_result.expr, cls_meta.cls_gen_name, prop_name)
        )

        | _ ->
          Format.eprintf "%s expr_type: %a\n" id.pident_name Core_type.TypeExpr.pp expr_type;
          failwith "can not find ctor of expr, maybe it's a property"
      )

      | _ -> failwith (Format.sprintf "unexpected: can not find member %s of id %d" id.pident_name expr.ty_var)
    )

    | Unary(op, expr) -> (
      let expr' = transform_expression ~is_borrow:true env expr in

      prepend_stmts := List.append !prepend_stmts expr'.prepend_stmts;
      append_stmts := List.append expr'.append_stmts !append_stmts;

      match op with
      | UnaryOp.Not ->
        Ir.Expr.Not expr'.expr

      | UnaryOp.Plus ->
        expr'.expr

      | UnaryOp.Minus ->
        Ir.Expr.I32Binary(BinaryOp.Mult, expr'.expr, Ir.Expr.NewInt "-1")

      | _ -> failwith "not implement"
    )

    | Binary (op, left, right) -> (
      let gen_c_op left right =
        let left' = transform_expression ~is_borrow:true env left in
        let right' = transform_expression ~is_borrow:true env right in

        prepend_stmts := List.concat [!prepend_stmts; left'.prepend_stmts; right'.prepend_stmts];
        append_stmts := List.concat [!append_stmts; left'.append_stmts; right'.append_stmts];

        let left_type = Type_context.deref_node_type env.ctx left.ty_var in

        let open Core_type in
        match (left_type, op) with
        | (TypeExpr.String, BinaryOp.Plus) ->
          let spec = auto_release_expr ~is_move env ~append_stmts ty_var
            (Ir.Expr. ExternalCall(SymLocal "lc_std_string_concat", None, [left'.expr; right'.expr]))
          in
          spec

        | (TypeExpr.String, BinaryOp.Equal)
        | (TypeExpr.String, BinaryOp.NotEqual)
        | (TypeExpr.String, BinaryOp.LessThan)
        | (TypeExpr.String, BinaryOp.LessThanEqual)
        | (TypeExpr.String, BinaryOp.GreaterThan)
        | (TypeExpr.String, BinaryOp.GreaterThanEqual)
          ->
          let spec = auto_release_expr ~is_move env ~append_stmts ty_var (Ir.Expr.StringCmp(op, left'.expr, right'.expr)) in
          spec

        | _ -> (
          (* let node_type = Type_context.deref_node_type env.ctx ty_var in *)
          if Check_helper.is_i64 env.ctx left_type then
            Ir.Expr.I64Binary(op, left'.expr, right'.expr)
          else if Check_helper.is_f64 env.ctx left_type then
            Ir.Expr.F64Binary(op, left'.expr, right'.expr)
          else if Check_helper.is_f32 env.ctx left_type then
            Ir.Expr.F32Binary(op, left'.expr, right'.expr)
          else
            Ir.Expr.I32Binary(op, left'.expr, right'.expr)
        )
      in

      let is_constant expression =
        match expression.spec with
        | Constant _ -> true
        | _ -> false
      in

      if is_constant left && is_constant right then
        (* if left and right are constants, try folding.
           if expr does not transformed by folding then
            generate c op directly by current left and right
           else
            try recursive transform new_expr *)
        let new_expr = try_folding_literals op left right expr in
        match new_expr with
        | Some(folded_expr) -> (
          let result = transform_expression ~is_borrow:true env folded_expr in
          result.expr
          )
        | None -> gen_c_op left right
      else (
        let left_res = try_folding_binary_expression left in
        let right_res = try_folding_binary_expression right in
        match (left_res, right_res) with
        | (None, None) ->
          (* if both left and right can't be fold, gen c op here  *)
            gen_c_op left right
        | _ -> (
          (* if `left` or `right` or both of them being transformed,
           we should do literal folding with updated expr too. *)
          let left_expr =
            match left_res with
            | Some(left') -> left'
            | None -> left
          in
          let right_expr =
            match right_res with
            | Some(right') -> right'
            | None -> right
          in
          let temp_expr = { expr with spec = Binary(op, left_expr, right_expr) } in
          let maybe_folded_expr = try_folding_literals op left_expr right_expr temp_expr in
          let expr =
            match maybe_folded_expr with
            | None -> temp_expr
            | Some(folded_expr) -> folded_expr
          in
          let result = transform_expression ~is_borrow:true env expr in
          result.expr
        )
      )
    )

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
    | Assign (op_opt, left_expr, expr) -> (
      let expr' = transform_expression ~is_move:true env expr in

      prepend_stmts := List.concat [ !prepend_stmts; expr'.prepend_stmts ];
      append_stmts := List.concat [ !append_stmts; expr'.append_stmts ];

      let assign_or_update main_expr ty_id =
        let node_type = Type_context.deref_node_type env.ctx ty_id in
        let need_release = not (Check_helper.type_should_not_release env.ctx node_type) in
        match op_opt with
        | None -> (
          if need_release then (
            let release_stmt = { Ir.Stmt.
              spec = Release main_expr;
              loc = Loc.none;
            } in
            prepend_stmts := List.append !prepend_stmts [release_stmt]
          );
          Ir.Expr.Assign(main_expr, expr'.expr)
        )

        | Some op ->
          Ir.Expr.Update(op, main_expr, expr'.expr)
      in

      match (left_expr, op_opt) with
      (* transform_expression env left_expr *)
      | ({ spec = Typedtree.Expression.Identifier (name, name_id); _ }, _) -> (
        let name = find_variable env name in
        assign_or_update (Ir.Expr.Ident name) name_id
      )

      (* TODO: maybe it's a setter? *)
      | ({ spec = Typedtree.Expression.Member (main_expr, id); _ }, _) -> (
        let transform_main_expr = transform_expression ~is_borrow:true env main_expr in

        prepend_stmts := List.concat [ !prepend_stmts; transform_main_expr.prepend_stmts ];
        append_stmts := List.concat [ !append_stmts; transform_main_expr.append_stmts ];

        (* let boxed_main_expr = prepend_expr ~is_borrow env ~prepend_stmts ~append_stmts transform_main_expr.expr in *)

        let main_expr_ty = Type_context.deref_node_type env.ctx main_expr.ty_var in
        let classname_opt = Check_helper.find_classname_of_type env.ctx main_expr_ty in
        let classname = Option.value_exn ~message:"can not find member of class" classname_opt in
        let gen_classname = find_variable env classname in
        let unwrap_name =
          match gen_classname with
          | Ir.SymLocal name -> name
          | _ -> failwith "unrechable"
        in

        assign_or_update
          (Ir.Expr.GetField(
            transform_main_expr.expr,
            unwrap_name,
            id.pident_name)
          )
          main_expr.ty_var
      )

      | ({ spec = Typedtree.Expression.Index (main_expr, value); _ }, None) -> (
        let transform_main_expr = transform_expression ~is_borrow:true env main_expr in

        (* let boxed_main_expr = prepend_expr env ~prepend_stmts ~append_stmts transform_main_expr.expr in *)
        let value_expr = transform_expression env value in

        prepend_stmts := List.concat [ !prepend_stmts; value_expr.prepend_stmts; transform_main_expr.prepend_stmts ];
        append_stmts := List.concat [ transform_main_expr.append_stmts; value_expr.append_stmts; !append_stmts; ];

        Ir.Expr.ExternalCall(SymLocal "LCArraySetValue", Some transform_main_expr.expr, [value_expr.expr; expr'.expr])
      )

      | _ ->
        Format.eprintf "Unexpected left: %a" Typedtree.Expression.pp left_expr;
        failwith "unreachable"
    )

    | Init { init_name; init_elements; _ } -> (
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;

      let init_name, init_name_id = init_name in

      let cls_meta = Hashtbl.find_exn env.cls_meta_map init_name_id in

      (* logical name -> real name *)

      let fun_name = find_variable env init_name in
      let init_call = Ir.Expr.InitCall((Ir.map_symbol ~f:(fun fun_name -> fun_name ^ "_init") fun_name)) in
      let init_cls_stmt = { Ir.Stmt.
        spec = Expr (
          Ir.Expr.Assign(
            (Temp tmp_id),
            init_call
          )
        );
        loc = Loc.none;
      } in

      let init_stmts =
        init_elements
        |> List.map
          ~f:(fun elm ->
            match elm with
            | InitEntry { init_entry_key; init_entry_value; init_entry_loc; _ } ->
              let actual_name = Hashtbl.find_exn cls_meta.cls_fields_map init_entry_key.pident_name in
              let transformed_value = transform_expression ~is_move:true env init_entry_value in
              let unwrap_name =
                match fun_name with
                | Ir.SymLocal name -> name
                | _ -> failwith "unrechable"
              in
              let left_value = (
                Ir.Expr.GetField(
                  (Temp tmp_id),
                  unwrap_name,
                  actual_name
                )
              ) in
              [{ Ir.Stmt.
                spec = Expr (
                  Assign(
                    left_value,
                    transformed_value.expr
                  )
                );
                loc = init_entry_loc;
              }]
            | InitSpread spread_expr -> (
              let transformed_spread = transform_expression ~is_move:true env spread_expr in
              let spread_expr' =
                if is_identifier spread_expr then
                  transformed_spread.expr
                else
                  prepend_expr env ~prepend_stmts ~append_stmts transformed_spread
              in
              transform_spreading_init env tmp_id spread_expr.ty_var spread_expr'
            )

          )
        |> List.concat
      in

      prepend_stmts := List.append !prepend_stmts (init_cls_stmt::init_stmts);

      if not is_move then (
        let release_stmt = { Ir.Stmt.
          spec = Release(Ir.Expr.Temp tmp_id);
          loc = Loc.none;
        } in
        append_stmts := List.append !append_stmts [release_stmt];
      );

      Ir.Expr.Temp tmp_id
    )

    | Block block ->
      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let tmp_var = Ir.SymTemp tmp_id in

      let init = { Ir.Stmt.
        spec = Expr (
          Ir.Expr.Assign (
            (Ident tmp_var),
            Null
          )
        );
        loc = Loc.none;
      } in

      let stmts = transform_block ~ret:tmp_var env block in

      prepend_stmts := List.concat [!prepend_stmts; [init]; stmts];

      Ir.Expr.Temp tmp_id

    | Match _match ->
      transform_pattern_matching env ~prepend_stmts ~append_stmts ~loc ~ty_var _match

    | This -> (
      if TScope.is_in_lambda env.scope then
        Ir.Expr.Ident SymLambdaThis
      else
        Ir.Expr.Ident SymThis
    )

    | Try try_expr -> (
      let expr_result = transform_expression ~is_move:true env try_expr in

      let expr' = prepend_expr env ~prepend_stmts ~append_stmts expr_result in

      let tmp_id = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;
      let tmp_var = "t[" ^ (Int.to_string tmp_id) ^ "]" in

      let stmt = { Ir.Stmt.
        spec = If {
          if_test = Ir.Expr.TagEqual (expr', 1);  (* 1 represent error *)
          if_consequent =
            List.append
            expr_result.append_stmts
            [{ Ir.Stmt.
                spec = Return (Some expr');
                loc = Loc.none;
            }];
          if_alternate = None;
        };
        loc = expr.loc;
      } in

      let union_get_expr = Ir.Expr.UnionGet(expr', 0) in

      let assign_stmt = { Ir.Stmt.
        spec = Expr (Ir.Expr.(Assign (Ident (Ir.SymLocal tmp_var), union_get_expr)));
        loc = expr.loc;
      } in

      prepend_stmts := List.concat [ !prepend_stmts; expr_result.prepend_stmts; [stmt; assign_stmt] ];
      append_stmts := List.append expr_result.append_stmts !append_stmts;

      Temp tmp_id
    )

    | Super -> failwith "unrechable"

    | Index(expr, index) -> (
      let expr' = transform_expression ~is_borrow:true env expr in
      let node_type = Type_context.deref_node_type env.ctx expr.ty_var in

      let index = transform_expression env index in

      prepend_stmts := List.concat [ !prepend_stmts; index.prepend_stmts; expr'.prepend_stmts ];
      append_stmts := List.concat [ expr'.append_stmts; index.append_stmts; !append_stmts ];

      match node_type with
      | String ->
        Ir.Expr.ExternalCall(Ir.SymLocal "lc_std_string_get_char", Some expr'.expr, [index.expr])

      | _ -> (
        let result = Ir.Expr.ArrayGetValue(expr'.expr, (Ir.Expr.IntValue index.expr)) in
        auto_release_expr env ~is_move ~append_stmts expr.ty_var result
      )

    )

  in
  {
    prepend_stmts = !prepend_stmts;
    expr = expr_spec;
    append_stmts = !append_stmts;
  }

and transform_pattern_matching env ~prepend_stmts ~append_stmts ~loc ~ty_var _match =
  let open Typedtree.Expression in
  let { match_expr; match_clauses; _ } = _match in
  let transformed_expr = transform_expression ~is_borrow:true env match_expr in
  let match_expr =
    if is_identifier match_expr then
      transformed_expr.expr
    else
      prepend_expr env ~prepend_stmts ~append_stmts (transform_expression ~is_borrow:true env match_expr)
  in
  let result_tmp = env.tmp_vars_count in
  env.tmp_vars_count <- env.tmp_vars_count + 1;

  prepend_stmts := List.append !prepend_stmts
    [{
      Ir.Stmt.
      spec = Expr (
        Ir.Expr.Assign (
          (Ident (Ir.SymLocal ("t[" ^ (Int.to_string result_tmp) ^ "]"))),
          Null
        );
      );
      loc = Loc.none;
    }];

  let tmp_counter = ref [] in
  let label_name = "done_" ^ (Int.to_string ty_var) in

  let is_last_goto stmts =
    let last_opt = List.last stmts in
    match last_opt with
    | Some { Ir.Stmt. spec = Goto _; _ } -> true
    | _ -> false
  in

  let rec transform_pattern_to_test match_expr pat : PMMeta.t =
    let open Pattern in
    let { spec; _ } = pat in
    match spec with
    | Underscore ->
      (fun genereator -> genereator ~finalizers:[] ())

    | Literal (Literal.Integer i) -> (
      let i_str = Int32.to_string i in
      let if_test = Ir.Expr.IntValue(I32Binary(BinaryOp.Equal, match_expr, NewInt i_str)) in
      (fun genereator ->
        let if_stmt = { Ir.Stmt.
          spec = If {
            if_test;
            if_consequent= genereator ~finalizers:[] ();
            if_alternate = None;
          };
          loc = Loc.none;
        } in
        [if_stmt]
      )
    )

    | Literal (Literal.String(str, _, _)) -> (
      let if_test = Ir.Expr.StringEqUtf8(match_expr, str) in
      (fun genereator ->
        let if_stmt = { Ir.Stmt.
          spec = If {
            if_test;
            if_consequent= genereator ~finalizers:[] ();
            if_alternate = None;
          };
          loc = Loc.none;
        } in
        [if_stmt]
      )
    )

    | Literal (Literal.Boolean bl) -> (
      let if_test =
        if bl then
          Ir.Expr.IntValue match_expr
        else
          Ir.Expr.IntValue(Not match_expr)
      in
      (fun genereator ->
        let if_stmt = { Ir.Stmt.
          spec = If {
            if_test;
            if_consequent= genereator ~finalizers:[] ();
            if_alternate = None;
          };
          loc = Loc.none;
        } in
        [if_stmt]
      )
    )

    | Literal (Literal.Char ch) -> (
      let if_test = Ir.Expr.IntValue(I32Binary(BinaryOp.Equal, match_expr, Ir.Expr.NewChar ch)) in
      (fun genereator ->
        let if_stmt = { Ir.Stmt.
          spec = If {
            if_test;
            if_consequent= genereator ~finalizers:[] ();
            if_alternate = None;
          };
          loc = Loc.none;
        } in
        [if_stmt]
      )
    )

    | Literal _ -> failwith "unimplemented"

    | Symbol (name, name_id) -> (
      let first_char = String.get name 0 in
      if Char.is_uppercase first_char then (
        let name_node = Type_context.get_node env.ctx name_id in
        let ctor_opt = Check_helper.find_typedef_of env.ctx name_node.value in
        let ctor = Option.value_exn ~message:(Format.sprintf "find enum ctor failed: %s %d" name name_id) ctor_opt in
        let enum_ctor = Core_type.TypeDef.(
          match ctor.spec with
          | EnumCtor v -> v
          | _ -> failwith "unrechable"
        ) in
        let if_test = Ir.Expr.TagEqual(match_expr, enum_ctor.enum_ctor_tag_id) in
        (fun genereator ->
          let if_stmt = { Ir.Stmt.
            spec = If {
              if_test;
              if_consequent= genereator ~finalizers:[] ();
              if_alternate = None;
            };
            loc = Loc.none;
          } in
          [if_stmt]
        )
      ) else ( (* binding local var *)
        let sym = find_variable env name in
        let assign_stmt = { Ir.Stmt.
          spec = Expr(Assign(Ident sym, match_expr));
          loc = Loc.none;
        } in
        (fun genereator ->
          let acc = genereator ~finalizers:[] () in
          assign_stmt::acc
        )
      )
    )
    | EnumCtor ((_name, name_id), child) -> (
      let name_node = Type_context.get_node env.ctx name_id in
      let ctor_opt = Check_helper.find_typedef_of env.ctx name_node.value in
      let ctor = Option.value_exn ctor_opt in
      let enum_ctor = Core_type.TypeDef.(
        match ctor.spec with
        | EnumCtor v -> v
        | _ -> failwith "n"
      ) in
      let if_test = Ir.Expr.TagEqual(match_expr, enum_ctor.enum_ctor_tag_id) in

      let match_tmp = env.tmp_vars_count in
      env.tmp_vars_count <- env.tmp_vars_count + 1;

      let open Ir.Expr in

      let assign_stmt = { Ir.Stmt.
        spec = Expr(Assign(Temp match_tmp, UnionGet(match_expr, 0)));
        loc = Loc.none;
      } in

      let release_stmt = { Ir.Stmt.
        spec = Release(Ir.Expr.Temp match_tmp);
        loc = Loc.none;
      } in
      
      let open PMMeta in
      let this_pm: t = fun generator ->
        let acc = generator ~finalizers:[release_stmt] () in
        let if_consequent=
          if is_last_goto acc then
            List.concat [
              [assign_stmt];
              acc;
            ]
          else
            List.concat [
              [assign_stmt];
              acc;
              [release_stmt];
            ]
        in
        let if_stmt = { Ir.Stmt.
          spec = If {
            if_test;
            if_consequent;
            if_alternate = None;
          };
          loc = Loc.none;
        } in
        [if_stmt]
      in
      let child_pm = transform_pattern_to_test (Temp match_tmp) child in
      this_pm >>= child_pm
    )

    | Tuple children -> (
      let open Ir in
      let assign_stmts, release_stmts, child_pms = 
        children
        |> List.mapi
          ~f:(fun index elm ->
            let match_tmp = env.tmp_vars_count in
            env.tmp_vars_count <- env.tmp_vars_count + 1;
            let assign_stmt = { Ir.Stmt.
              spec = Expr(Assign(Temp match_tmp, TupleGetValue(match_expr, index)));
              loc = Loc.none;
            } in

            let release_stmt = { Ir.Stmt.
              spec = Release(Expr.Temp match_tmp);
              loc = Loc.none;
            } in

            let child_pm = transform_pattern_to_test (Temp match_tmp) elm in

            assign_stmt, release_stmt, child_pm
          )
        |> List.unzip3
      in
      let open PMMeta in
      let this_pm: t = fun generator ->
        let acc = generator ~finalizers:release_stmts () in
        if is_last_goto acc then
          List.concat [
            assign_stmts;
            acc;
          ]
        else
          List.concat [
            assign_stmts;
            acc;
            release_stmts;
          ]
      in
      List.fold
        ~init:this_pm
        ~f:(fun acc elm -> acc >>= elm)
        child_pms
    )

    | Array { elements; rest; } -> (
      let elm_len = List.length elements in
      let open Ir in
      let need_test =
        Expr.IntValue (
        match rest with
        | Some _ ->
          Expr.I32Binary(
            BinaryOp.GreaterThanEqual,
            Expr.ExternalCall(SymLocal "lc_std_array_get_length", Some match_expr, []),
            Expr.NewInt (Int.to_string elm_len))

        | None ->
          Expr.I32Binary(
            BinaryOp.Equal,
            Expr.ExternalCall(SymLocal "lc_std_array_get_length", Some match_expr, []),
            Expr.NewInt (Int.to_string elm_len))
      )

      in

      let assign_stmts, release_stmts, child_pms = 
        elements
        |> List.mapi
          ~f:(fun index elm ->
            let match_tmp = env.tmp_vars_count in
            env.tmp_vars_count <- env.tmp_vars_count + 1;
            let assign_stmt = { Ir.Stmt.
              spec = Expr(Assign(Temp match_tmp, ArrayGetValue(match_expr, Expr.IntValue(Expr.NewInt (Int.to_string index)))));
              loc = Loc.none;
            } in

            let release_stmt = { Ir.Stmt.
              spec = Release(Expr.Temp match_tmp);
              loc = Loc.none;
            } in

            let child_pm = transform_pattern_to_test (Temp match_tmp) elm in

            assign_stmt, release_stmt, child_pm
          )
        |> List.unzip3
      in

      let rest_assigns, rest_releases, rest_pms =
        match rest with
        | Some { spec = Underscore; _ } -> [], [], []
        | Some rest_pat -> (
          let match_tmp = env.tmp_vars_count in
          env.tmp_vars_count <- env.tmp_vars_count + 1;

          let assign_stmt = { Ir.Stmt.
            spec = Expr(Assign(
              Temp match_tmp,
              ExternalCall(
                SymLocal "lc_std_array_slice",
                Some match_expr,
                [Expr.NewInt (Int.to_string elm_len); Expr.ExternalCall(SymLocal "lc_std_array_get_length", Some match_expr, [])])));
            loc = Loc.none;
          } in

          let release_stmt = { Ir.Stmt.
            spec = Release(Expr.Temp match_tmp);
            loc = Loc.none;
          } in

          let rest_pm = transform_pattern_to_test (Temp match_tmp) rest_pat in

          [assign_stmt], [release_stmt], [rest_pm]
        )

        | None ->
          [], [], []

      in

      let open PMMeta in
      let this_pm: t = fun generator ->
        let acc = generator ~finalizers:(List.append rest_releases release_stmts) () in
        let if_consequent =
          if is_last_goto acc then
            List.concat [
              assign_stmts;
              rest_assigns;
              acc;
            ]
          else
            List.concat [
              assign_stmts;
              rest_assigns;
              acc;
              rest_releases;
              release_stmts;
            ]
        in
        let if_stmt = { Ir.Stmt.
          spec = If {
            if_test = need_test;
            if_consequent;
            if_alternate = None;
          };
          loc = Loc.none;
        } in
        [if_stmt]
      in
      List.fold
        ~init:this_pm
        ~f:(fun acc elm -> acc >>= elm)
        (List.append child_pms rest_pms)
    )

  in

  let transform_clause clause =
    let scope = create_scope_and_distribute_vars env clause.clause_scope in
    with_scope env scope (fun env ->
      let saved_tmp_count = env.tmp_vars_count in
      let body = transform_expression ~is_move:true env clause.clause_consequent in

      let done_stmt = { Ir.Stmt.
        spec = Goto label_name;
        loc = clause.clause_loc;
      } in

      let consequent ~finalizers () : Ir.Stmt.t list = List.concat [
        body.prepend_stmts;
        [{ Ir.Stmt.
          spec = Expr (
            Assign(
              (Ident (Ir.SymLocal ("t[" ^ (Int.to_string result_tmp) ^ "]"))),
              body.expr
            )
          );
          loc = Loc.none;
        }];
        body.append_stmts;
        finalizers;
        [done_stmt];
      ] in

      let pm_meta = transform_pattern_to_test match_expr clause.clause_pat in
      let pm_stmts = pm_meta consequent in

      prepend_stmts := List.append
        !prepend_stmts
        pm_stmts;
      tmp_counter := env.tmp_vars_count::(!tmp_counter);
      env.tmp_vars_count <- saved_tmp_count;
    )
  in

  List.iter ~f:transform_clause match_clauses;

  let end_label = { Ir.Stmt.
    spec = Label label_name;
    loc;
  } in
  prepend_stmts := List.append !prepend_stmts [end_label];

  (* use the max tmp vars *)
  env.tmp_vars_count <- List.fold ~init:0 ~f:(fun acc item -> if item > acc then item else acc) !tmp_counter;

  Ir.Expr.Temp result_tmp

and transform_spreading_init env tmp_id spread_ty_var spread_expr =
  let expr_node_type = Type_context.deref_node_type env.ctx spread_ty_var in

  let ctor_opt = Check_helper.find_construct_of env.ctx expr_node_type in
  let ctor, _ = Option.value_exn
    ~message:(Format.asprintf "Can not find ctor of spreading init: %d %a" spread_ty_var Core_type.TypeExpr.pp expr_node_type)
    ctor_opt
  in
  let cls_id = ctor.id in

  let cls_meta = Hashtbl.find_exn env.cls_meta_map cls_id in

  cls_meta.cls_fields
  |> List.map
    ~f:(fun (field_name, _) ->
    let left_value = (
      Ir.Expr.GetField(
        (Temp tmp_id),
        cls_meta.cls_gen_name,
        field_name
      )
    ) in
    let get_field = Ir.Expr.GetField(spread_expr, cls_meta.cls_gen_name, field_name) in
    [{ Ir.Stmt.
      spec = Retain get_field;
      loc = Loc.none;
    };
    { Ir.Stmt.
      spec = Expr (
        Assign(
          left_value,
          get_field
        )
      );
      loc = Loc.none;
    }]
  )
  |> List.concat

and transform_expression_if env ?ret ~prepend_stmts ~append_stmts loc if_desc =
  let open Typedtree.Expression in

  let test = transform_expression env if_desc.if_test in

  let if_test = Ir.Expr.IntValue test.expr in
  let consequent = transform_block ?ret env if_desc.if_consequent in
  let if_alternate =
    Option.map
    ~f:(fun alt ->
      match alt with
      | If_alt_if if_spec ->
        Ir.Stmt.If_alt_if (transform_expression_if env ?ret ~prepend_stmts ~append_stmts loc if_spec)
      | If_alt_block blk ->
        let consequent = transform_block ?ret env blk in
        Ir.Stmt.If_alt_block consequent
    )
    if_desc.if_alternative
  in
  
  let spec = {
    Ir.Stmt.
    if_test;
    if_consequent = consequent;
    if_alternate;
  } in

  prepend_stmts := List.concat [!prepend_stmts; test.prepend_stmts];
  append_stmts := List.append test.append_stmts !append_stmts;
  spec

and transform_block env ?ret (block: Typedtree.Block.t): Ir.Stmt.t list =
  let block_scope = create_scope_and_distribute_vars env block.scope in
  with_scope env block_scope (fun env ->
    let stmts = List.map ~f:(transform_statement ?ret env) block.body |> List.concat in

    let cleanup = generate_finalize_stmts env.scope in

    List.append stmts cleanup
  )

(*
 * for normal blocks,
 * only the vars in the current blocks should be released
 *)
and generate_finalize_stmts scope =
  let open TScope in
  !(scope.local_vars_to_release)
  |> List.rev
  |> List.filter_map
    ~f:(fun sym ->
      Some { Ir.Stmt.
        spec = Release (Ident sym);
        loc = Loc.none;
      }
    )

(*
 * for return statements,
 * all the variables in the function should be released
 *)
and generate_finalize_stmts_function scope =
  TScope.get_symbols_to_release_til_function [] scope
  |> List.rev
  |> List.filter_map
    ~f:(fun sym ->
      Some { Ir.Stmt.
        spec = Release (Ident sym);
        loc = Loc.none;
      }
    )

and generate_finalize_stmts_while scope =
  TScope.get_symbols_to_release_til_while [] scope
  |> List.rev
  |> List.filter_map
    ~f:(fun sym ->
      Some { Ir.Stmt.
        spec = Release (Ident sym);
        loc = Loc.none;
      }
    )

and generate_cls_meta env cls_id gen_name =
  let node = Type_context.get_node env.ctx cls_id in
  let ctor_opt = Check_helper.find_typedef_of env.ctx node.value in
  let ctor = Option.value_exn ctor_opt in

  let rec generate_properties_including_ancester child (cls_def: Core_type.TypeDef.t) =
    let cls_def =
      match cls_def with
      | { spec = Class cls; _ } -> cls
      | _ -> failwith "unexpected: not a class"
    in
    let properties =
      List.filter_map
        ~f:(fun (elm_name, elm) ->
          match elm with
          | Cls_elm_prop (_, ty_var, _) -> 
            Some (elm_name, ty_var)
          | _ -> None
        )
        cls_def.tcls_elements;
    in

    let result = List.append properties child in
    match cls_def.tcls_extends with
    | Some ancester -> (
      let ctor_opt = Check_helper.find_construct_of env.ctx ancester in
      let ctor, _ = Option.value_exn ctor_opt in
      generate_properties_including_ancester result ctor
    )

    | _ -> result

  in

  let prop_names = generate_properties_including_ancester [] ctor in

  let result = create_cls_meta cls_id gen_name prop_names in

  List.iter
    ~f:(fun (field_name, _) -> Hashtbl.set result.cls_fields_map ~key:field_name ~data:field_name)
    result.cls_fields;

  result

and transform_class_method env _method : (Ir.Decl.t list * Ir.Decl.class_method_tuple option) =
  let open Declaration in
  let { cls_method_name; cls_method_params; cls_method_body; cls_method_scope; _ } = _method in

  env.tmp_vars_count <- 0;

  let origin_method_name, method_id = cls_method_name in
  let new_name =
    match Hashtbl.find_exn env.global_name_map method_id with
    | Ir.SymLocal name -> name
    | _ -> failwith "unrechable"
  in

  let open Core_type in
  let node_type = Type_context.deref_node_type env.ctx method_id in
  let is_virtual =
    match node_type with
    | TypeExpr.TypeDef { spec = TypeDef.ClassMethod { method_is_virtual = true; _ }; _ } ->
      true
    | _ -> false
  in

  let tuple =
    if is_virtual then
      Some { Ir.Decl.
        class_method_name = origin_method_name;
        class_method_gen_name = new_name;
      }
    else
      None
  in

  let _fun = { Ir.Decl.
    spec = (transform_function_impl env
      ~name:new_name
      ~params:cls_method_params
      ~scope:(Option.value_exn cls_method_scope)
      ~body:cls_method_body
      ~comments:[]);
    loc = _method.cls_method_loc;
  } in

  let lambdas = env.lambdas in
  env.lambdas <- [];

  (* will be reversed in the future *)
  (_fun::lambdas), tuple

and distribute_name_to_class_method env cls_original_name _method =
  let open Declaration in
  let { cls_method_name; _ } = _method in
  let origin_method_name, method_id = cls_method_name in
  let new_name = cls_original_name ^ "_" ^ origin_method_name in
  let new_name = distribute_name env new_name in

  let pre_declare = { Ir.Decl.
    spec = FuncDecl (SymLocal new_name);
    loc = Loc.none;
  } in
  env.prepends_decls <- pre_declare::(env.prepends_decls);

  Hashtbl.set env.global_name_map ~key:method_id ~data:(SymLocal new_name)

and transform_class env cls loc: Ir.Decl.t list =
  let open Declaration in
  let { cls_id; cls_body; _ } = cls in
  let original_name, cls_name_id = cls_id in
  let fun_name = distribute_name env original_name in
  let finalizer_name = fun_name ^ "_finalizer" in
  let gc_marker_name = fun_name ^ "_marker" in

  env.current_fun_meta <- Some (create_current_fun_meta original_name);

  Hashtbl.set env.global_name_map ~key:cls_name_id ~data:(SymLocal fun_name);

  let _, cls_id' = cls_id in
  let cls_meta = generate_cls_meta env cls_id' fun_name in
  Hashtbl.set env.cls_meta_map ~key:cls_meta.cls_id ~data:cls_meta;

  (*
   * Prescan:
   *
   * Distributes names to all methods,
   * because the body of all methods will call the method of class itself
   *
   *)
  List.iter
    ~f:(fun elm->
      match elm with
      | Cls_method _method -> distribute_name_to_class_method env original_name _method
      | _ -> ()
    )
    cls_body.cls_body_elements;

  let class_methods = ref [] in

  let methods: Ir.Decl.t list = 
    List.fold
      ~init:[]
      ~f:(fun acc elm ->
        match elm with
        | Cls_method _method -> (
          let stmts, tuple_opt = transform_class_method env _method in

          (match tuple_opt with
          | Some tuple ->
            class_methods := tuple::!class_methods
          | None -> ()
          );

          (* will be reversed in the future *)
          List.append stmts acc
        )
        | Cls_property _
        | Cls_declare _ -> acc
      )
      cls_body.cls_body_elements;
  in

  env.class_inits <- { Ir.Decl.
    class_name = fun_name;
    class_id_name = fun_name ^ "_class_id";
    class_def_name = fun_name ^ "_def";
    class_methods = List.rev !class_methods;
  }::env.class_inits;

  let _, cls_id' = cls_id in
  let cls_type = Type_context.deref_node_type env.ctx cls_id' in
  let cls_typedef = Check_helper.find_typedef_of env.ctx cls_type in
  let finalizer = generate_finalizer env finalizer_name (Option.value_exn cls_typedef) in
  let gc_marker = generate_gc_marker env gc_marker_name (Option.value_exn cls_typedef) in

  let cls = {
    Ir.Decl.
    name = fun_name;
    original_name;
    finalizer;
    gc_marker;
    properties = cls_meta.cls_fields;
  } in

  List.append
    [ { Ir.Decl. spec = Ir.Decl.Class cls; loc; } ]
    (List.rev methods)

(*
 * Generate finalizer statements for a class:
 * If a class has ancesters, generate the ancesters's statements first.
 *)
and generate_finalizer env name (type_def: Core_type.TypeDef.t) : Ir.Decl.class_finalizer option =
  let generate_release_statements_by_cls_meta type_def =
    let open Core_type.TypeDef in
    let cls_meta = Hashtbl.find_exn env.cls_meta_map type_def.id in
    List.filter_map
      ~f:(fun (field_name, ty_var) ->
        let field_type = Type_context.deref_node_type env.ctx ty_var in
        if Check_helper.type_should_not_release env.ctx field_type then
          None
        else
          Some {
            Ir.Stmt.
            spec = Release (Ir.Expr.RawGetField("ptr", field_name));
            loc = Loc.none;
          }
      )
      cls_meta.cls_fields
  in

  let finalizer_content = generate_release_statements_by_cls_meta type_def in

  if List.is_empty finalizer_content then
    None
  else
    Some {
      finalizer_name = name;
      finalizer_content;
    }

and generate_gc_marker env name (type_def: Core_type.TypeDef.t) : Ir.Decl.gc_marker option =
  let generate_marker_fields type_def =
    let open Core_type.TypeDef in
    let cls_meta = Hashtbl.find_exn env.cls_meta_map type_def.id in
    List.filter_map
      ~f:(fun (field_name, ty_var) ->
        let field_type = Type_context.deref_node_type env.ctx ty_var in
        if Check_helper.type_is_not_gc env.ctx field_type then
          None
        else
          Some field_name
          
      )
      cls_meta.cls_fields
  in

  let fields = generate_marker_fields type_def in
  
  if List.is_empty fields then
    None
  else
    Some {
      gc_marker_name = name;
      gc_marker_field_names = fields;
    }

and transform_enum env enum loc : Ir.Decl.t list =
  let open Enum in
  let { elements; name = (enum_original_name, _); _ } = enum in

  env.current_fun_meta <- Some (create_current_fun_meta enum_original_name);

  let result =
    elements
    |> List.mapi
      ~f:(fun index elm ->
        match elm with
        | Typedtree.Enum.Case case -> (
          let case_name, case_name_id = case.case_name in
          let new_name = distribute_name env case_name in
          Hashtbl.set env.global_name_map ~key:case_name_id ~data:(SymLocal new_name);
          let spec = Ir.Decl.EnumCtor {
            enum_ctor_name = new_name;
            enum_ctor_tag_id = index;
            enum_ctor_params_size = List.length case.case_fields;
          } in
          [{ Ir.Decl. spec; loc }]
        )
        | Typedtree.Enum.Method _method ->
          distribute_name_to_class_method env enum_original_name _method;
          let stmts, _ = transform_class_method env _method in
          stmts

      )
    |> List.concat
  in

  env.current_fun_meta <- None;

  result

and transform_lambda env ~lambda_name content _ty_var =
  let prev_fun_meta = env.current_fun_meta in

  env.current_fun_meta <- Some (create_current_fun_meta lambda_name);

  let lambda_gen_name = "LCC_" ^ lambda_name in

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
  let result =
    {
      Ir.Decl.
      spec = _fun;
      loc = Loc.none;
    }
  in

  env.current_fun_meta <- prev_fun_meta;

  result;

type result = {
  main_function_name: string option;
  declarations: Ir.Decl.t list;
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
        Ir.Decl.
        spec = GlobalClassInit(name, List.rev env.class_inits);
        loc = Loc.none;
      }]), Some name
    )
  in

  let prepends_decls = List.rev env.prepends_decls in
  env.prepends_decls <- [];

  {
    main_function_name = env.main_function_name;
    declarations = List.append prepends_decls declarations;
    global_class_init;
  }
