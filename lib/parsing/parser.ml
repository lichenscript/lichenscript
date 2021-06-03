
open Ast
open Parser_env
open Waterlang_lex

let rec parse_string source content = 
  let env = Parser_env.init_env source content in
  let program = parse_program env in
  let errs = errors env in
  if List.length errs > 0 then
    List.iter
      (fun (_, err) ->
        let msg = Parse_error.PP.error err in
        print_endline msg)
      errs
  else 
    print_endline "no errors"
  ;
  program

and parse_program env : program =
  let stmt = parse_statement env in
  {
    pprogram_statements = [
      stmt;
    ];
    pprogram_comments = [];
  }

and parse_statement env : statement =
  let pstmt_loc = Peek.loc env in
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
    pstmt_loc;
    pstmt_loc_stack = [];
    pstmt_attributes = [];
  }

and parse_identifier env : Identifier.t =
  let next = Peek.token env in
  let pident_loc = Peek.loc env in
  match next with
  | Token.T_IDENTIFIER ident ->
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
  let id = parse_identifier env in
  let pcls_loc = Peek.loc env in
  {
    pcls_id = Some id;
    pcls_loc;
    pcls_body = Pcls_method;
    pcls_comments = [];
  }

and parse_expression env : expression =
  let loc = Peek.loc env in
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
    pexp_loc = loc;
    pexp_loc_stack = [];
    pexp_attributes = [];
  }

and _parse_pattern env : pattern =
  let ppat_loc = Peek.loc env in
  let next = Peek.token env in
  let ppat_desc =
    match next with
    | Token.T_IDENTIFIER _ ->
      let ident = parse_identifier env in
      Ppat_identifier ident

    | _ -> failwith "not implemented"
  in

  { ppat_loc; ppat_desc; }
