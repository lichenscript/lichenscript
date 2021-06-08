
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
  let stmt = parse_statement env in
  {
    pprogram_statements = [
      stmt;
    ];
    pprogram_comments = [];
  }

and parse_statement env : statement =
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  let pstmt_desc: statement_desc =
    match next with
    | Token.T_CLASS ->
      Pstmt_class (parse_class env)

    | Token.T_IF ->
      Eat.token env;
      let test = parse_expression env in
      let consequent = parse_statement env in
      let has_else = Eat.maybe env Token.T_ELSE in
      let alternative = if has_else then Some (parse_statement env) else None in
      Pstmt_if (test, consequent, alternative, [])
      
    | _ -> failwith "not implementd"

  in

  {
    pstmt_desc;
    pstmt_loc = with_start_loc env start_loc;
    pstmt_loc_stack = [];
    pstmt_attributes = [];
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
        Eat.token env;
        Expect.token env Token.T_RPAREN;
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
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  let pexp_desc : expression_desc =
    match next with 
    | Token.T_IDENTIFIER _ ->
      let ident = parse_identifier env in
      Pexp_identifier ident
    
    | _ ->
      failwith "not implemented"

  in

  {
    pexp_desc;
    pexp_loc = with_start_loc env start_loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

and _parse_pattern env : pattern =
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
