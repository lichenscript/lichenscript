
open Ast
open Parser_env
open Waterlang_lex

let with_start_loc env start_loc =
  let loc = last_loc env in
  match loc with
  | Some loc -> Loc.btwn start_loc loc
  | None -> start_loc

let rec parse_string source content = 
  let env = Parser_env.init_env source content in
  let program = parse_program env in
  let errs = errors env in
  if List.length errs > 0 then
    Result.Error errs
  else 
    Result.Ok program

and parse_program env : program =
  let stmts = ref [] in

  while Peek.token env <> Token.T_EOF do
    let stmt = parse_statement env in
    stmts := stmt::(!stmts)
  done;

  {
    pprogram_statements = List.rev !stmts;
    pprogram_comments = [];
  }

and parse_statement env : Statement.t =
  let open Statement in
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  let spec: Statement.spec =
    match next with
    | Token.T_CLASS ->
      Class (parse_class env)

    | Token.T_FUNCTION ->
      Function_ (parse_function env)

    | Token.T_CONST ->
      Binding (parse_var_binding env Pvar_const)

    | Token.T_LET ->
      Binding (parse_var_binding env Pvar_let)

    | Token.T_SEMICOLON ->
      Eat.token env;
      Empty

    | Token.T_WHILE ->
      begin
        let start_loc = Peek.loc env in
        Eat.token env;
        Expect.token env Token.T_LPAREN;
        let while_test = parse_expression env in
        Expect.token env Token.T_RPAREN;
        let while_block = parse_block env in
        let while_loc = with_start_loc env start_loc in
        While {
          while_test;
          while_block;
          while_loc;
        }
      end

    | Token.T_BREAK ->
      Eat.token env;
      let next = Peek.token env in
      let id =
        match next with
        | Token.T_IDENTIFIER _ ->
          Some (parse_identifier env)
        | _ -> None
      in
      Expect.token env Token.T_SEMICOLON;
      Break id

    | Token.T_CONTINUE ->
      Eat.token env;
      let next = Peek.token env in
      let id =
        match next with
        | Token.T_IDENTIFIER _ ->
          Some (parse_identifier env)
        | _ -> None
      in
      Expect.token env Token.T_SEMICOLON;
      Contintue id

    | Token.T_DEBUGGER ->
      Eat.token env;
      Debugger

    | Token.T_RETURN ->
      Eat.token env;
      let next = Peek.token env in
      if next == Token.T_SEMICOLON then
        begin
          Eat.token env;
          Return None
        end
      else
        begin
          let expr = parse_expression env in
          Return(Some expr)
        end

    | _ ->
      let expr = parse_expression env in
      if Peek.token env == Token.T_SEMICOLON then
        Semi expr
      else
        Expr expr

  in

  {
    spec;
    loc = with_start_loc env start_loc;
    loc_stack = [];
    attributes = [];
  }

and parse_var_binding env kind: Statement.var_binding =
  let start_loc = Peek.loc env in
  Expect.token env Token.T_LET;
  let binding_pat = parse_pattern env in

  let binding_ty =
    if Peek.token env == Token.T_COLON then
      begin
        Eat.token env;
        Some (parse_type env)
      end
    else None
  in

  Expect.token env Token.T_ASSIGN;
  let binding_init = parse_expression env in
  Expect.token env Token.T_SEMICOLON;
  {
    binding_kind = kind;
    binding_loc = with_start_loc env start_loc;
    binding_ty;
    binding_pat;
    binding_init;
  }

and parse_function env: Function.t =
  let open Function in
  let start_loc = Peek.loc env in
  Expect.token env Token.T_FUNCTION;
  let id = parse_identifier env in
  let params = parse_params env in
  let next = Peek.token env in
  let return_ty =
    match next with
    | Token.T_COLON ->
      begin
        Eat.token env;
        let ty = parse_type env in
        Some ty
      end

    | _ -> None
  in
  let block = parse_block env in
  {
    id = Some id;
    params;
    return_ty;
    body = Fun_block_body block;
    loc = with_start_loc env start_loc;
    comments = [];
  }

and parse_params env: Function.params =
  let parse_param env: Function.param =
    let start_loc = Peek.loc env in
    let param_rest = Eat.maybe env Token.T_ELLIPSIS in
    let param_pat = parse_pattern env in
    let param_ty =
      if Peek.token env == Token.T_COLON then
        begin
          Eat.token env;
          Some (parse_type env)
        end
      else
        None
    in
    {
      param_pat;
      param_ty;
      param_init = None;
      param_loc = with_start_loc env start_loc;
      param_rest;
    }
  in

  let start_loc = Peek.loc env in
  Expect.token env Token.T_LPAREN;

  let content = ref [] in

  while Peek.token env <> Token.T_RPAREN do
    content := (parse_param env)::(!content);
    if Peek.token env <> Token.T_RPAREN then (
      Expect.token env Token.T_COMMA;
    )
  done;

  Expect.token env Token.T_RPAREN;
  {
    params_content = List.rev (!content);
    params_loc = with_start_loc env start_loc;
  }

and parse_block env: Block.t =
  let start_pos = Peek.loc env in
  let content = ref [] in

  Expect.token env Token.T_LCURLY;

  while Peek.token env <> Token.T_RCURLY do
    content := (parse_statement env)::(!content);
  done;

  Expect.token env Token.T_RCURLY;
  {
    body = List.rev (!content);
    loc = with_start_loc env start_pos;
  }

and parse_identifier env : Identifier.t =
  let next = Peek.token env in
  let pident_loc = Peek.loc env in
  match next with
  | Token.T_IDENTIFIER ident ->
    Eat.token env;
    {
      Identifier.
      pident_name = ident.value;
      pident_loc;
    }

  | _ ->
    let expected = "identitifer" in
    error_unexpected ~expected env;
    Eat.token env;
    { Identifier.
      pident_name = "<unexpected>";
      pident_loc;
    }

and parse_class env : Statement._class =
  let start_loc = Peek.loc env in
  Eat.token env;  (* class *)
  let id = parse_identifier env in
  let body = parse_class_body env in
  {
    cls_id = Some id;
    cls_loc = with_start_loc env start_loc;
    cls_body = body;
    cls_comments = [];
  }

and parse_visibility env =
  let next = Peek.token env in
  match next with
  | Token.T_PUBLIC ->
    Eat.token env;
    Some Pvisibility_public

  | Token.T_PROTECTED ->
    Eat.token env;
    Some Pvisibility_protected

  | Token.T_PRIVATE ->
    Eat.token env;
    Some Pvisibility_private

  | _ -> None

and parse_class_body env: Statement.class_body =
  let open Statement in
  let parse_element env =
    let start_pos = Peek.loc env in
    let v = parse_visibility env in
    let id = parse_identifier env in

    if Peek.token env == Token.T_LPAREN then
      begin
        Eat.token env; (* ( *)
        Expect.token env Token.T_RPAREN; (* ) *)
        Cls_method {
          cls_method_visiblity = v;
          cls_method_name = id;
          cls_method_loc = with_start_loc env start_pos;
        }
      end
    else
      begin
        let cls_property_type = 
          if Peek.token env == Token.T_COLON then
            begin
              Eat.token env;
              Some (parse_type env)
            end
          else
            None
        in

        let cls_property_init =
          if Peek.token env == Token.T_ASSIGN then
            begin
              Eat.token env;
              Some (parse_expression env)
            end
          else
            None
        in

        Expect.token env Token.T_SEMICOLON;
        Cls_property {
          cls_property_visiblity = v;
          cls_property_loc = with_start_loc env start_pos;
          cls_property_name = id;
          cls_property_type;
          cls_property_init;
        }
    end
  in

  let start_loc = Peek.loc env in
  Expect.token env Token.T_LCURLY;  (* { *)

  let tmp_body: class_body_element list ref = ref [] in

  while Peek.token env <> Token.T_RCURLY && Peek.token env <> Token.T_EOF do
    tmp_body := (parse_element env)::!tmp_body;
  done;

  Expect.token env Token.T_RCURLY;  (* } *)
  {
    cls_body_elements = List.rev !tmp_body;
    cls_body_loc = with_start_loc env start_loc;
  }

and parse_expression env : Expression.t =
  parse_binary_expression env

and parse_binary_expression env : Expression.t =
  let open Expression in
  let rec parse_binary_enhance env left_expr left_token =
    let start_loc = Peek.loc env in
    let expr = parse_exponentialtion_expression env in
    let left_prec = Precedence.binary_precedence left_token in
    let right_token = Peek.token env in
    let right_prec = Precedence.binary_precedence right_token in

    if left_prec < right_prec then (
      Eat.token env;
      let right = parse_binary_enhance env expr right_token in
      let op = Asttypes.BinaryOp.from_token left_token in
      let spec = Binary(op, left_expr, right) in
      {
        spec;
        loc = with_start_loc env start_loc;
        loc_stack = [];
        attributes = [];
      }
    ) else if left_prec == right_prec then (
      if left_prec <= 0 then
        expr
      else (
        let op = Asttypes.BinaryOp.from_token left_token in
        let spec = Binary(op, left_expr, expr) in
        let binary =
          {
            spec;
            loc = with_start_loc env start_loc;
            loc_stack = [];
            attributes = [];
          }
        in
        parse_binary_enhance env binary right_token
      )
    ) else (
      let op = Asttypes.BinaryOp.from_token left_token in
      let spec = Binary(op, left_expr, expr) in
      let binary =
        {
          spec;
          loc = with_start_loc env start_loc;
          loc_stack = [];
          attributes = [];
        }
      in
      if right_prec <= 0 then
        binary
      else (
        Eat.token env;
        parse_binary_enhance env binary right_token
      )
    )
  in

  let expr = parse_exponentialtion_expression env in
  let next = Peek.token env in
  let prec = Precedence.binary_precedence next in
  if prec > 0 then
    begin
      Eat.token env;
      parse_binary_enhance env expr next
    end
  else
    expr

and parse_exponentialtion_expression env =
  parse_unary_expression env

and parse_unary_expression env: Expression.t =
  let open Expression in
  let start_loc = Peek.loc env in
  let next = Peek.token env in
  match next with
  | Token.T_PLUS
  | Token.T_MINUS
  | Token.T_BIT_NOT
  | Token.T_NOT ->
    begin
      let op = Asttypes.UnaryOp.from_token next in
      Eat.token env;
      let expr = parse_unary_expression env in
      let spec = Unary(op, expr) in
      {
        spec;
        loc = with_start_loc env start_loc;
        loc_stack = [];
        attributes = [];
      }
    end

  | _ ->
    parse_update_expression env

and parse_update_expression env : Expression.t =
  let open Expression in
  let token_to_op =
    let open Asttypes.UpdateOp in
    function
      | Token.T_INCR -> Increase
      | Token.T_DECR -> Decrease
      | _ -> failwith "unreachable"
  in
  let next = Peek.token env in
  match next with
  | Token.T_INCR
  | Token.T_DECR ->
    begin
      let start_loc = Peek.loc env in
      Eat.token env;
      let expr = parse_unary_expression env in
      let op = token_to_op next in
      let spec = Update(op, expr, true) in
      {
        spec;
        loc = with_start_loc env start_loc;
        loc_stack = [];
        attributes = [];
      }
    end
  | _ ->
    let start_loc = Peek.loc env in
    let expr = parse_left_handside_expression_allow_call env in
    let next = Peek.token env in
    match next with
    | Token.T_INCR
    | Token.T_DECR ->
      begin
        Eat.token env;
        let op = token_to_op next in
        let spec = Update(op, expr, false) in
        {
          spec;
          loc = with_start_loc env start_loc;
          loc_stack = [];
          attributes = [];
        }
      end

    | _ ->
      expr

and parse_arguments env =
  let result = ref [] in

  Expect.token env Token.T_LPAREN;

  while (Peek.token env) <> Token.T_RPAREN do
    let expr = parse_expression env in
    result := expr::!result;
    
    if (Peek.token env) <> Token.T_RPAREN then (
      Expect.token env Token.T_COMMA;
    );
  done;

  Expect.token env Token.T_RPAREN;

  List.rev !result

and parse_left_handside_expression_allow_call env : Expression.t =
  let open Expression in
  let rec loop env expr =
    let start_pos = Peek.loc env in
    let next = Peek.token env in
    match next with
    | Token.T_PERIOD ->  (* . *)
      Eat.token env;
      let id = parse_identifier env in
      let spec = Member(expr, id) in
      let expr =
        {
          spec;
          loc = with_start_loc env start_pos;
          loc_stack = [];
          attributes = [];
        }
      in
      loop env expr

    | Token.T_LPAREN ->  (* ( )*)
      let call_params = parse_arguments env in
      let call =
        {
          callee = expr;
          call_params;
          call_loc = with_start_loc env start_pos;
        }
      in
      let spec = Call call in
      let expr =
        {
          spec;
          loc = with_start_loc env start_pos;
          loc_stack = [];
          attributes = [];
        }
      in
      loop env expr

    (* | Token.T_LCURLYBAR *)

    | _ -> expr

  in

  let expr = parse_primary_expression env in
  loop env expr

and parse_primary_expression env : Expression.t =
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  let spec : Expression.spec =
    match next with 
    | Token.T_IDENTIFIER _ ->
      let ident = parse_identifier env in
      Identifier ident

    | Token.T_NUMBER { raw; _ } ->
      Eat.token env;
      Constant (Pconst_integer (raw, None))

    | Token.T_STRING (loc, value, _, _) ->
      Eat.token env;
      Constant (Pconst_string (value, loc, None))

    | Token.T_TRUE ->
      Eat.token env;
      Constant (Pconst_boolean true)

    | Token.T_FALSE ->
      Eat.token env;
      Constant (Pconst_boolean false)

    | Token.T_THROW ->
      Eat.token env;
      Throw (parse_expression env)

    | Token.T_IF ->
      begin
        let start_loc = Peek.loc env in
        Eat.token env;
        let if_test = parse_expression env in
        let if_consequent = parse_statement env in
        let has_else = Eat.maybe env Token.T_ELSE in
        let if_alternative = if has_else then Some (parse_statement env) else None in
        If {
          if_test;
          if_consequent;
          if_alternative;
          if_loc = with_start_loc env start_loc;
        }
      end

    | Token.T_LBRACKET ->
      begin
        let result = ref [] in
        Eat.token env;

        while Peek.token env <> Token.T_RBRACKET do
          let expr = parse_expression env in

          if Peek.token env <> Token.T_RBRACKET then (
            Expect.token env Token.T_COMMA;
          );

          result := expr::(!result);
        done;

        Expect.token env Token.T_RBRACKET;
        Array (List.rev !result)
      end

    | _ ->
      let tok = Token.token_to_string next in
      let str = Format.sprintf "not implemented %s" tok in
      failwith str

  in

  {
    spec;
    loc = with_start_loc env start_loc;
    loc_stack = [];
    attributes = [];
  }

and parse_pattern env : Pattern.t =
  let open Pattern in
  let start_loc = Peek.loc env in
  let next = Peek.token env in
  let spec =
    match next with
    | Token.T_IDENTIFIER _ ->
      let ident = parse_identifier env in
      Identifier ident

    | _ -> failwith "not implemented"
  in

  {
    spec;
    loc = with_start_loc env start_loc;
  }

and parse_type env : Type.t =
  let open Type in
  let start_loc = Peek.loc env in
  let id = parse_identifier env in
  {
    spec = Ty_ctor (id, []);
    loc = with_start_loc env start_loc;
  }
