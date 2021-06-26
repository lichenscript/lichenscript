
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

and parse_statement env : statement =
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  let pstmt_desc: statement_desc =
    match next with
    | Token.T_CLASS ->
      Pstmt_class (parse_class env)

    | Token.T_FUNCTION ->
      Pstmt_function (parse_function env)

    | Token.T_CONST ->
      Pstmt_binding (parse_var_binding env Pvar_const)

    | Token.T_LET ->
      Pstmt_binding (parse_var_binding env Pvar_let)

    | Token.T_SEMICOLON ->
      Eat.token env;
      Pstmt_empty

    | Token.T_WHILE ->
      begin
        let start_loc = Peek.loc env in
        Eat.token env;
        Expect.token env Token.T_LPAREN;
        let pwhile_test = parse_expression env in
        Expect.token env Token.T_RPAREN;
        let pwhile_block = parse_block env in
        let pwhile_loc = with_start_loc env start_loc in
        Pstmt_while {
          pwhile_test;
          pwhile_block;
          pwhile_loc;
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
      Pstmt_break id

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
      Pstmt_contintue id

    | Token.T_DEBUGGER ->
      Eat.token env;
      Pstmt_debugger

    | Token.T_RETURN ->
      Eat.token env;
      let next = Peek.token env in
      if next == Token.T_SEMICOLON then
        begin
          Eat.token env;
          Pstmt_return None
        end
      else
        begin
          let expr = parse_expression env in
          Pstmt_return(Some expr)
        end

    | _ ->
      let expr = parse_expression env in
      if Peek.token env == Token.T_SEMICOLON then
        Pstmt_semi expr
      else
        Pstmt_expr expr

  in

  {
    pstmt_desc;
    pstmt_loc = with_start_loc env start_loc;
    pstmt_loc_stack = [];
    pstmt_attributes = [];
  }

and parse_var_binding env kind =
  let start_loc = Peek.loc env in
  Expect.token env Token.T_LET;
  let pbinding_pat = parse_pattern env in

  let pbinding_ty =
    if Peek.token env == Token.T_COLON then
      begin
        Eat.token env;
        Some (parse_type env)
      end
    else None
  in

  Expect.token env Token.T_ASSIGN;
  let pbinding_init = parse_expression env in
  Expect.token env Token.T_SEMICOLON;
  {
    pbinding_kind = kind;
    pbinding_loc = with_start_loc env start_loc;
    pbinding_ty;
    pbinding_pat;
    pbinding_init;
  }

and parse_function env: _function =
  let start_loc = Peek.loc env in
  Expect.token env Token.T_FUNCTION;
  let id = parse_identifier env in
  let pfun_params = parse_params env in
  let block = parse_block env in
  {
    pfun_id = Some id;
    pfun_params;
    pfun_body = Pfun_block_body block;
    pfun_loc = with_start_loc env start_loc;
    pfun_comments = [];
  }

and parse_params env =
  let parse_param env =
    let start_loc = Peek.loc env in
    let pparam_rest = Eat.maybe env Token.T_ELLIPSIS in
    let pparam_pat = parse_pattern env in
    let pparam_ty =
      if Peek.token env == Token.T_COLON then
        begin
          Eat.token env;
          Some (parse_type env)
        end
      else
        None
    in
    {
      pparam_pat;
      pparam_ty;
      pparam_init = None;
      pparam_loc = with_start_loc env start_loc;
      pparam_rest;
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
    pparams_content = List.rev (!content);
    pparams_loc = with_start_loc env start_loc;
  }

and parse_block env =
  let start_pos = Peek.loc env in
  let content = ref [] in

  Expect.token env Token.T_LCURLY;

  while Peek.token env <> Token.T_RCURLY do
    content := (parse_statement env)::(!content);
  done;

  Expect.token env Token.T_RCURLY;
  {
    pblk_body = List.rev (!content);
    pblk_loc = with_start_loc env start_pos;
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

and parse_class env : _class =
  let start_loc = Peek.loc env in
  Eat.token env;  (* class *)
  let id = parse_identifier env in
  let body = parse_class_body env in
  {
    pcls_id = Some id;
    pcls_loc = with_start_loc env start_loc;
    pcls_body = body;
    pcls_comments = [];
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

and parse_class_body env =
  let parse_element env =
    let start_pos = Peek.loc env in
    let v = parse_visibility env in
    let id = parse_identifier env in

    if Peek.token env == Token.T_LPAREN then
      begin
        Eat.token env; (* ( *)
        Expect.token env Token.T_RPAREN; (* ) *)
        Pcls_method {
          pcls_method_visiblity = v;
          pcls_method_name = id;
          pcls_method_loc = with_start_loc env start_pos;
        }
      end
    else
      begin
        let pcls_property_type = 
          if Peek.token env == Token.T_COLON then
            begin
              Eat.token env;
              Some (parse_type env)
            end
          else
            None
        in

        let pcls_property_init =
          if Peek.token env == Token.T_ASSIGN then
            begin
              Eat.token env;
              Some (parse_expression env)
            end
          else
            None
        in

        Expect.token env Token.T_SEMICOLON;
        Pcls_property {
          pcls_property_visiblity = v;
          pcls_property_loc = with_start_loc env start_pos;
          pcls_property_name = id;
          pcls_property_type;
          pcls_property_init;
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
    pcls_body_elements = List.rev !tmp_body;
    pcls_body_loc = with_start_loc env start_loc;
  }

and parse_expression env : expression =
  parse_binary_expression env

and parse_binary_expression env =
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
      let desc = Pexp_binary(op, left_expr, right) in
      {
        pexp_desc = desc;
        pexp_loc = with_start_loc env start_loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      }
    ) else if left_prec == right_prec then (
      if left_prec <= 0 then
        expr
      else (
        let op = Asttypes.BinaryOp.from_token left_token in
        let desc = Pexp_binary(op, left_expr, expr) in
        let binary =
          {
            pexp_desc = desc;
            pexp_loc = with_start_loc env start_loc;
            pexp_loc_stack = [];
            pexp_attributes = [];
          }
        in
        parse_binary_enhance env binary right_token
      )
    ) else (
      let op = Asttypes.BinaryOp.from_token left_token in
      let desc = Pexp_binary(op, left_expr, expr) in
      let binary =
        {
          pexp_desc = desc;
          pexp_loc = with_start_loc env start_loc;
          pexp_loc_stack = [];
          pexp_attributes = [];
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

and parse_unary_expression env =
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
      let pexp_desc = Pexp_unary(op, expr) in
      {
        pexp_desc;
        pexp_loc = with_start_loc env start_loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
      }
    end

  | _ ->
    parse_update_expression env

and parse_update_expression env =
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
      let pexp_desc = Pexp_update(op, expr, true) in
      {
        pexp_desc;
        pexp_loc = with_start_loc env start_loc;
        pexp_loc_stack = [];
        pexp_attributes = [];
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
        let pexp_desc = Pexp_update(op, expr, false) in
        {
          pexp_desc;
          pexp_loc = with_start_loc env start_loc;
          pexp_loc_stack = [];
          pexp_attributes = [];
        }
      end

    | _ ->
      expr

and parse_left_handside_expression_allow_call env =
  parse_primary_expression env

and parse_primary_expression env =
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  let pexp_desc : expression_desc =
    match next with 
    | Token.T_IDENTIFIER _ ->
      let ident = parse_identifier env in
      Pexp_identifier ident

    | Token.T_NUMBER { raw; _ } ->
      Eat.token env;
      Pexp_constant (Pconst_integer (raw, None))

    | Token.T_STRING (loc, value, _, _) ->
      Eat.token env;
      Pexp_constant (Pconst_string (value, loc, None))

    | Token.T_TRUE ->
      Eat.token env;
      Pexp_constant (Pconst_boolean true)

    | Token.T_FALSE ->
      Eat.token env;
      Pexp_constant (Pconst_boolean false)

    | Token.T_THROW ->
      Eat.token env;
      Pexp_throw (parse_expression env)

    | Token.T_IF ->
      begin
        let start_loc = Peek.loc env in
        Eat.token env;
        let pif_test = parse_expression env in
        let pif_consequent = parse_statement env in
        let has_else = Eat.maybe env Token.T_ELSE in
        let pif_alternative = if has_else then Some (parse_statement env) else None in
        Pexp_if {
          pif_test;
          pif_consequent;
          pif_alternative;
          pif_loc = with_start_loc env start_loc;
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
        Pexp_array (List.rev !result)
      end

    | _ ->
      let tok = Token.token_to_string next in
      let str = Format.sprintf "not implemented %s" tok in
      failwith str

  in

  {
    pexp_desc;
    pexp_loc = with_start_loc env start_loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

and parse_pattern env : pattern =
  let start_loc = Peek.loc env in
  let next = Peek.token env in
  let ppat_desc =
    match next with
    | Token.T_IDENTIFIER _ ->
      let ident = parse_identifier env in
      Ppat_identifier ident

    | _ -> failwith "not implemented"
  in

  {
    ppat_loc = with_start_loc env start_loc;
    ppat_desc;
  }

and parse_type env : _type =
  let start_loc = Peek.loc env in
  let id = parse_identifier env in
  {
    pty_desc = Pty_ctor (id, []);
    pty_loc = with_start_loc env start_loc;
  }
