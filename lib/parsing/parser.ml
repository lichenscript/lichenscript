(*
 * This file is part of LichenScript Compiler.
 *
 * LichenScript Compiler is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * LichenScript Compiler is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with LichenScript Compiler. If not, see <https://www.gnu.org/licenses/>.
 *)
open Ast
open Parser_env
open Lichenscript_lex

type parse_result = {
  tree: Ast.program;
  include_module_ids: string list;
}

(*
 * Relaxed params
 *
 * () => {}
 * (): unit => {}
 * (a: string, b: i32) => ()
 * (a: string, b: i32) : () => {}
 * (Init {})
 * (1 + 1)
 *)
module ReleaxedArrow = struct
  type arrow ={
    arrow_expr: Expression.t;
    arrow_loc: Loc.t
  }

  type param = {
    param_expr: Expression.t;
    param_colon: (Type.t * Loc.t) option;
    param_loc: Loc.t
  }

  type t = {
    params: param list;
    params_loc: Loc.t;
    return_ty: Type.t option;
    arrow: arrow option;
    loc: Loc.t;
  }
  
end

let with_start_loc env start_loc =
  let loc = last_loc env in
  match loc with
  | Some loc -> Loc.btwn start_loc loc
  | None -> start_loc

let rec parse_attribute env : attribute =
  let rec parse_payload env list =
    match Peek.token env with
    | T_STRING(_, value, _, _) ->
      let next_list = value::list in
      Eat.token env;
      if (Peek.token env) <> Token.T_RPAREN then (
        Expect.token env Token.T_COMMA
      );
      parse_payload env next_list

    | T_RPAREN ->
      Eat.token env;
      List.rev list

    | _ ->
      Expect.error env (Peek.token env);
      List.rev list
  in
  let start_loc = Peek.loc env in
  Expect.token env Token.T_AT;
  let id = parse_identifier_with_keywords env in
  let attrib: string Asttypes.loc =
    { Asttypes.
      txt = id.pident_name;
      loc = with_start_loc env start_loc;
    }
  in
  let attr_payload =
    if Peek.token env == Token.T_LPAREN then (
      Eat.token env;
      parse_payload env []
    ) else
      []
  in
  {
    attr_name = attrib;
    attr_payload;
    attr_loc = with_start_loc env start_loc;
  }

and parse_attributes env : attributes =
  let result = ref [] in
  while (Peek.token env) == Token.T_AT do
    let attrib = parse_attribute env in
    result := attrib::!(result)
  done;
  List.rev !result

and parse_string source content = 
  let env = Parser_env.init_env source content in
  let program = parse_program env in
  let errs = errors env in
  if List.length errs > 0 then
    Result.Error errs
  else  (
    let include_module_ids = Parser_env.include_module_ids env in
    Result.Ok {
      tree = program;
      include_module_ids;
    }
  )

and parse_program env : program =
  let decls = ref [] in
  let start_loc = Peek.loc env in

  while Peek.token env <> Token.T_EOF do
    let decl = parse_declaration env in
    decls := decl::(!decls)
  done;

  {
    pprogram_top_level = Parser_env.get_top_level env;
    pprogram_declarations = List.rev !decls;
    pprogram_comments = [];
    pprogram_loc = with_start_loc env start_loc;
  }

and parse_declaration env : Declaration.t =
  let open Declaration in
  let start_loc = Peek.loc env in
  let attributes = parse_attributes env in
  let next = Peek.token env in
  let spec: Declaration.spec =
    match next with
    | Token.T_IMPORT -> (
      Eat.token env;
      let perr_loc = Peek.loc env in
      let next = Peek.token env in
      match next with
      | Token.T_STRING (loc, content, _, _) -> (
        Import {
          source = content;
          source_loc = loc;
        }
      )

      | _ -> (
        let perr_spec = Parser_env.get_unexpected_error next in
        Parse_error.error {
          perr_spec;
          perr_loc;
        }
      )
    )

    (* | Token.T_PUBLIC
    | Token.T_PROTECTED
    | Token.T_PRIVATE ->
      begin
        let open Asttypes in
        let visibility =
          match next with
          | Token.T_PUBLIC -> Pvisibility_public
          | Token.T_PROTECTED -> Pvisibility_protected
          | Token.T_PRIVATE -> Pvisibility_private
          | _ -> failwith "unreachable"
        in
        Eat.token env;
        if Parse_scope.((scope env).ty = Parse_scope.PScope_Module) then (
          let next = Peek.token env in
          match next with
          | Token.T_FUNCTION -> (
            let fun_ = parse_function ~visibility env in
            let { Function. header = { id; _ }; _ } = fun_ in
            let name = id.pident_name in

            Parser_env.add_top_level env ~name ~visibility;

            Function_ fun_
          )

          | _ -> (
            let lex_error = Lichenscript_lex.Lex_error.Unexpected (Token.value_of_token (Peek.token env)) in
            let parse_error = {
              Parse_error.
              perr_spec = Parse_error.LexError lex_error;
              perr_loc = (Peek.loc env);
            } in

            Parse_error.error parse_error
          )
        ) else (
          let err =
            {
              Parse_error.
              perr_loc = start_loc;
              perr_spec = Parse_error.VisibilityNoOnTopLevel;
            }
          in
          Parse_error.error err
        )
      end *)

    | _ -> (
      let open Asttypes in
      let visibility =
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
      in
      let next = Peek.token env in
      match next with
      | Token.T_CLASS ->
        Class (parse_class ~visibility env)

      | Token.T_FUNCTION -> (
        let _fun = parse_function ~visibility env in

        let { Function. header = { id; _ }; _ } = _fun in
        let name = id.pident_name in

        Parser_env.add_top_level env ~name ~visibility;

        Function_ _fun
      )

      | Token.T_INTERFACE -> (
        let intf = parse_interface env ~visibility in

        let name = intf.intf_name.pident_name in
        Parser_env.add_top_level env ~name ~visibility;

        Interface intf
      )

      | Token.T_DECLARE ->
        begin
          Eat.token env;
          let function_header = parse_function_header env in
          let spec = Declaration.DeclFunction function_header in

          let name = function_header.id.pident_name in
          Parser_env.add_top_level env ~name ~visibility;

          if Peek.token env = Token.T_SEMICOLON then (
            Eat.token env
          );

          Declaration.Declare {
            decl_visibility = visibility;
            decl_spec = spec;
            decl_loc = with_start_loc env start_loc;
          }
        end

      | Token.T_ENUM ->
        let parse_case env =
          let start_loc = Peek.loc env in
          Expect.token env Token.T_CASE;
          let case_name = parse_identifier env in
          Parser_env.add_top_level env ~name:(case_name.pident_name) ~visibility;
          let fields =
            if (Peek.token env) == Token.T_LPAREN then (
              let result = ref [] in
              Eat.token env;

              while (Peek.token env) <> Token.T_RPAREN do
                let ty = parse_type env in
                result := ty::(!result);
                if (Peek.token env) <> Token.T_RPAREN then (
                  Expect.token env Token.T_COMMA
                )
              done;

              Expect.token env Token.T_RPAREN;
              List.rev !result
            ) else
              []
          in

          { Enum.
            case_name;
            case_fields = fields;
            case_loc = with_start_loc env start_loc;
          }
        in
        let start_loc = Peek.loc env in
        Eat.token env;
        let name = parse_identifier env in

        let type_vars =
          if Peek.token env = Token.T_LESS_THAN then
            parse_type_vars env
          else
            []
        in

        let cases = ref [] in
        Expect.token env Token.T_LCURLY;

        while (Peek.token env) <> Token.T_RCURLY && (Peek.token env) <> Token.T_EOF do
          let _case = parse_case env in
          cases := _case::(!cases);
        done;

        Expect.token env Token.T_RCURLY;

        Parser_env.add_top_level env ~name:(name.pident_name) ~visibility;

        Enum { Enum.
          visibility;
          name;
          type_vars;
          cases = List.rev !cases;
          loc = with_start_loc env start_loc;
        }

      | _ ->
        let lex_error = Lichenscript_lex.Lex_error.Unexpected (Token.value_of_token (Peek.token env)) in
        let parse_error = {
          Parse_error.
          perr_spec = Parse_error.LexError lex_error;
          perr_loc = (Peek.loc env);
        } in

        Parse_error.error parse_error
    )

  in
  {
    spec;
    loc = with_start_loc env start_loc;
    attributes;
  }

and parse_interface env ~visibility : Declaration.intf =
  Expect.token env Token.T_INTERFACE;
  let name = parse_identifier env in

  let intf_type_vars =
    if (Peek.token env) = Token.T_LESS_THAN then
      parse_type_vars env
    else []
  in

  let parse_method env =
    let start_loc = Peek.loc env in
    let first_id = parse_identifier env in
    let open Declaration in
    let intf_method_get_set =
      match first_id.pident_name with
      | "get" -> Some Cls_getter
      | "set" -> Some Cls_setter
      | _ -> None
    in
    (*
      * if next is identifier, the first one is get/set
      *)
    let intf_method_name =
      match (Peek.token env) with
      | Token.T_IDENTIFIER _ -> parse_identifier env
      | _ -> first_id
    in

    let intf_method_params = parse_params env in

    let intf_method_return_ty =
      match (Peek.token env) with
      | Token.T_COLON ->
        begin
          Eat.token env;
          let ty = parse_type env in
          Some ty
        end

      | _ -> None
    in

    {
      intf_method_get_set;
      intf_method_name;
      intf_method_params;
      intf_method_loc = with_start_loc env start_loc;
      intf_method_return_ty;
    }
  in

  let methods = ref [] in
  Expect.token env Token.T_LCURLY;

  while (Peek.token env) <> Token.T_RCURLY do
    let _method = parse_method env in
    if (Peek.token env) = Token.T_SEMICOLON then (
      Eat.token env
    );
    methods := _method::(!methods)
  done;

  Expect.token env Token.T_RCURLY;

  {
    intf_visibility = visibility;
    intf_name = name;
    intf_type_vars;
    intf_methods = List.rev !methods;
  }

and parse_statement env : Statement.t =
  let open Statement in
  let start_loc = Peek.loc env in
  let attributes = parse_attributes env in
  let next = Peek.token env in

  let spec: Statement.spec =
    match next with
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
        let while_test = parse_expression env in
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
      Continue id

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
          ignore (Eat.maybe env Token.T_SEMICOLON);
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
    attributes;
  }

and parse_type_vars env =
  let result = ref [] in
  Expect.token env Token.T_LESS_THAN;

  while (Peek.token env) <> Token.T_GREATER_THAN do
    let id = parse_identifier env in
    result := id::!result;
    if (Peek.token env) = Token.T_COMMA then (
      Eat.token env
    )
  done;

  Expect.token env Token.T_GREATER_THAN;
  List.rev !result

and parse_var_binding env kind: Statement.var_binding =
  let start_loc = Peek.loc env in
  (match kind with
  | Pvar_const ->
    Expect.token env Token.T_CONST
  | Pvar_let ->
    Expect.token env Token.T_LET
  );
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

and parse_function_header env: Function.header =
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
  { Function.
    id;
    params;
    return_ty;
    header_loc = with_start_loc env start_loc;
  }

and parse_function ~visibility env: Function.t =
  let open Function in
  let prev_scope = scope env in
  let fun_scope = Parse_scope.create ~prev:prev_scope Parse_scope.PString_Function in
  with_scope env fun_scope (fun () ->
    let start_loc = Peek.loc env in
    let header = parse_function_header env in
    let block = parse_block env in

    (* let name = header.id.pident_name in *)
    (* Parser_env.add_top_level env ~name ~visibility:(Option.value ~default:Asttypes.Pvisibility_private visibility); *)

    {
      visibility;
      header;
      body = block;
      loc = with_start_loc env start_loc;
      comments = [];
    }
  )

and parse_params env: Function.params =
  let parse_param env: Function.param =
    let start_loc = Peek.loc env in
    let param_rest = Eat.maybe env Token.T_ELLIPSIS in
    let param_name = parse_identifier env in
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
      param_name;
      param_ty;
      param_loc = with_start_loc env start_loc;
      param_rest;
    }
  in

  let start_loc = Peek.loc env in
  Expect.token env Token.T_LPAREN;

  let content = ref [] in

  while (Peek.token env) <> Token.T_RPAREN && (Peek.token env) <> Token.T_EOF do
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

  Expect.token env Token.T_LCURLY;

  let rec parse_content rst =
    match Peek.token env with
    | Token.T_RCURLY ->
      Eat.token env;
      List.rev rst

    | Token.T_EOF -> List.rev rst
    | _ ->
      (
        let stmt = parse_statement env in
        parse_content (stmt::rst)
      )
  in

  {
    body = parse_content [];
    loc = with_start_loc env start_pos;
  }

and parse_identifier env : Identifier.t =
  let next = Peek.token env in
  let start_loc = Peek.loc env in
  match next with
  | Token.T_IDENTIFIER ident ->
    Eat.token env;
    {
      Identifier.
      pident_name = ident.value;
      pident_loc = with_start_loc env start_loc;
    }

  | _ ->
    let expected = "identitifer" in
    error_unexpected ~expected env;
    Eat.token env;
    { Identifier.
      pident_name = "<unexpected>";
      pident_loc = with_start_loc env start_loc;
    }

and parse_identifier_with_keywords env : Identifier.t =
  let next = Peek.token env in
  let start_loc = Peek.loc env in
  if Token.is_keyword next then
    let value = Token.value_of_token next in
    Eat.token env;
    {
      Identifier.
      pident_name = value;
      pident_loc = with_start_loc env start_loc;
    }
  else
    parse_identifier env

and parse_class ~visibility env : Declaration._class =
  let start_loc = Peek.loc env in
  Eat.token env;  (* class *)
  let id = parse_identifier env in

  let name = id.pident_name in
  Parser_env.add_top_level env ~name ~visibility;

  let cls_type_vars =
    if (Peek.token env) = Token.T_LESS_THAN then
      parse_type_vars env
    else []
  in

  let cls_extends =
    if Peek.token env = Token.T_EXTENDS then (
      Eat.token env;
      let ext_ty = parse_type env in
      Some ext_ty
    ) else
      None
  in

  let cls_implements =
    if Peek.token env = Token.T_IMPLEMENTS then (
      Eat.token env;
      let first_intf = parse_type env in
      let tys = ref [ first_intf ] in

      while (Peek.token env) = Token.T_COMMA do
        Eat.token env;
        let next = parse_type env in
        tys := next::(!tys)
      done;

      List.rev !tys
    ) else []
  in

  let body = parse_class_body env in
  {
    cls_visibility = visibility;
    cls_extends;
    cls_implements;
    cls_id = id;
    cls_type_vars;
    cls_loc = with_start_loc env start_loc;
    cls_body = body;
    cls_comments = [];
  }

and parse_visibility env =
  let open Asttypes in
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

and parse_class_body env: Declaration.class_body =
  let open Declaration in
  let parse_element env =
    let start_pos = Peek.loc env in
    let attributes =
      if (Peek.token env) = Token.T_AT then
        parse_attributes env
      else
        []
    in

    if (Peek.token env) = Token.T_DECLARE then (
      Eat.token env;
      let first_id = parse_identifier env in
      let cls_decl_method_get_set =
        match first_id.pident_name with
        | "get" -> Some Cls_getter
        | "set" -> Some Cls_setter
        | _ -> None
      in
      (*
       * if next is identifier, the first one is get/set
       *)
      let cls_decl_method_name =
        match (Peek.token env) with
        | Token.T_IDENTIFIER _ -> parse_identifier env
        | _ -> first_id
      in
      let params = parse_params env in
      let cls_decl_method_return_ty =
        if Peek.token env = Token.T_COLON then (
          Eat.token env;
          Some (parse_type env)
        ) else
          None
      in
      Expect.token env Token.T_SEMICOLON;
      Cls_declare {
        cls_decl_method_attributes = attributes;
        cls_decl_method_get_set;
        cls_decl_method_name;
        cls_decl_method_params = params;
        cls_decl_method_loc = with_start_loc env start_pos;
        cls_decl_method_return_ty;
      }
    ) else
      let v = parse_visibility env in

      let cls_method_modifier =
        match Peek.token env with
        | Token.T_STATIC ->
          Eat.token env;
          Some Ast.Declaration.Cls_modifier_static

        | Token.T_VIRTUAL ->
          Eat.token env;
          Some Ast.Declaration.Cls_modifier_virtual

        | Token.T_OVERRIDE ->
          Eat.token env;
          Some Ast.Declaration.Cls_modifier_override

        | _ -> None
      in

      let id = parse_identifier env in

      if Peek.token env = Token.T_LPAREN then
        begin
          let params = parse_params env in
          let cls_method_return_ty =
            if Peek.token env = Token.T_COLON then (
              Eat.token env;
              Some (parse_type env)
            ) else
              None
          in
          let cls_method_body = parse_block env in
          Cls_method {
            cls_method_attributes = attributes;
            cls_method_modifier;
            cls_method_visibility = v;
            cls_method_name = id;
            cls_method_params = params;
            cls_method_body;
            cls_method_loc = with_start_loc env start_pos;
            cls_method_return_ty;
          }
        end
      else
        begin
          Expect.token env Token.T_COLON;
          let cls_property_type = parse_type env in

          if (Peek.token env) = Token.T_SEMICOLON then (
            Eat.token env;
          );
          Cls_property {
            cls_property_attributes = attributes;
            cls_property_visibility = v;
            cls_property_loc = with_start_loc env start_pos;
            cls_property_name = id;
            cls_property_type;
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
  let start_loc = Peek.loc env in
  let expr = parse_assignment_expression env in
  match expr.spec with
  | Expression.Identifier id -> (
    match Peek.token env with
    | Token.T_LCURLY -> (
      Eat.token env;
      let init_elements = ref [] in

      while (Peek.token env) <> Token.T_RCURLY do
        let start_loc = Peek.loc env in
        let element =
          match (Peek.token env) with
          | Token.T_ELLIPSIS -> (
            Eat.token env;
            let expr = parse_expression env in
            Expression.InitSpread expr
          )
          | _ ->  (
            let init_entry_key = parse_identifier env in
            let init_entry_value =
              if (Peek.token env) = Token.T_COLON then (
                Eat.token env;
                Some (parse_expression env)
              ) else
                None
            in
            Expression.InitEntry {
              Expression.
              init_entry_key;
              init_entry_value;
              init_entry_loc = with_start_loc env start_loc;
            }
          )
        in
        init_elements := element::(!init_elements);
        if (Peek.token env) <> Token.T_RCURLY then (
          Expect.token env T_COMMA
        )
      done;

      Expect.token env Token.T_RCURLY;
      { expr with
        spec = Init {
          init_name = id;
          init_elements = List.rev !init_elements;
          init_loc = with_start_loc env start_loc;
        }
      }
    )
  | _ -> expr
  )

  | _ -> expr

(* and reinterpret_expression_as_id _env (expr: Expression.t) : Identifier.t =
  let { Expression. spec; loc; _; } = expr in
  match spec with
  | Identifier id ->
    id

  | _ ->
    let err =
      { Parse_error.
        perr_loc = loc;
        perr_spec = Parse_error.IsNotLeftValue;
      }
    in
    Parse_error.error err *)

and parse_assignment_expression env : Expression.t =
  let start_pos = Peek.loc env in
  let left = parse_binary_expression env in
  let next = Peek.token env in
  let op, ok =
    match next with
    | Token.T_ASSIGN -> None, true
    | Token.T_PLUS_ASSIGN -> (Some Asttypes.AssignOp.PlusAssign), true
    | Token.T_MINUS_ASSIGN -> (Some Asttypes.AssignOp.MinusAssign), true
    | Token.T_MULT_ASSIGN -> (Some Asttypes.AssignOp.MinusAssign), true
    | Token.T_DIV_ASSIGN -> (Some Asttypes.AssignOp.DivAssign), true
    | Token.T_MOD_ASSIGN -> (Some Asttypes.AssignOp.ModAssign), true
    | Token.T_LSHIFT_ASSIGN -> (Some Asttypes.AssignOp.LShiftAssign), true
    | Token.T_RSHIFT_ASSIGN -> (Some Asttypes.AssignOp.RShiftAssign), true
    | Token.T_BIT_OR_ASSIGN -> (Some Asttypes.AssignOp.BitOrAssign), true
    | Token.T_BIT_XOR_ASSIGN -> (Some Asttypes.AssignOp.BitXorAssign), true
    | Token.T_BIT_AND_ASSIGN -> (Some Asttypes.AssignOp.BitAndAssign), true

    | _ -> None, false
  in
  if ok then (
    Eat.token env;
    (* let left = reinterpret_expression_as_id env expr in *)
    let right = parse_assignment_expression env in
    let spec = Expression.Assign(op, left, right) in
    {
      spec;
      loc = with_start_loc env start_pos;
      attributes = [];
    }
  ) else
    left

and parse_maybe_arrow_function env : Expression.t =
  let relaxed_arrow = parse_relaxed_arrow env in
  (* cast to arrow function *)
  match relaxed_arrow.arrow with
  | Some arrow ->
    let open Expression in
    let lambda_params = {
      Function.
      params_content = List.map
        (fun param ->
          let open ReleaxedArrow in
          let param_name =
            match param.param_expr.spec with
            | Expression.Identifier id ->
              id
            | _ ->
              failwith "use identifier as arrow functions's param"
          in
          { Function.
            param_name;
            param_ty = Option.map (fun (ty, _) -> ty) param.param_colon;
            param_loc = param.param_loc;
            param_rest = false;
          }
        )
        relaxed_arrow.params;
      params_loc = relaxed_arrow.params_loc;
    } in
    let open ReleaxedArrow in
    let lambda_body = arrow.arrow_expr in
    let lambda = {
      lambda_body;
      lambda_params;
      lambda_return_ty = ReleaxedArrow.(relaxed_arrow.return_ty);
    } in
    { Expression.
      spec = Lambda lambda;
      loc = relaxed_arrow.loc;
      attributes = [];
    }

  | None -> (
    match relaxed_arrow.params with
    | first::[] -> (
      (match first.param_colon with
      | Some (_, perr_loc) ->
        let tok = Token.token_to_string Token.T_COLON in
        let lex_error = Lichenscript_lex.Lex_error.Unexpected tok in
        let perr_spec = Parse_error.LexError lex_error in
        let err =
          { Parse_error.
            perr_loc;
            perr_spec;
          }
        in
        Parse_error.error err

      | None -> ()
      );

      first.param_expr
    )

    | _ -> (
      let tok = Token.token_to_string Token.T_LPAREN in
      let lex_error = Lichenscript_lex.Lex_error.Unexpected tok in
      let perr_spec = Parse_error.LexError lex_error in
      let err =
        { Parse_error.
          perr_loc = relaxed_arrow.params_loc;
          perr_spec;
        }
      in
      Parse_error.error err
    )

  )

and parse_relaxed_arrow env : ReleaxedArrow.t =
  let start_pos = Peek.loc env in

  let parse_relaxed_param () =
    let start_pos = Peek.loc env in
    let param_expr = parse_expression env in
    let param_colon =
      if (Peek.token env) = Token.T_COLON then (
        let start_pos = Peek.loc env in
        Eat.token env;
        Some (parse_type env, with_start_loc env start_pos)
      )
      else
        None
    in
    { ReleaxedArrow.
      param_expr;
      param_colon;
      param_loc = with_start_loc env start_pos;
    }
  in

  let params = ref [] in

  Expect.token env Token.T_LPAREN;

  while (Peek.token env) <> Token.T_RPAREN do
    let param = parse_relaxed_param () in
    params := param::!params
  done;

  Expect.token env Token.T_RPAREN;
  let params_loc = with_start_loc env start_pos in

  let return_ty =
    if (Peek.token env) = Token.T_COLON then (
      Eat.token env;
      Some (parse_type env)
    ) else
      None
  in

  let arrow =
    if (Peek.token env) = Token.T_ARROW then (
      let start_pos = Peek.loc env in
      Eat.token env;
      let arrow_expr = parse_expression env in
      let arrow = {
        ReleaxedArrow.
        arrow_expr;
        arrow_loc = with_start_loc env start_pos;
      } in
      Some arrow
    ) else
      None
  in

  { ReleaxedArrow.
    params = List.rev !params;
    params_loc;
    return_ty;
    arrow;
    loc = with_start_loc env start_pos;
  }

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
          attributes = [];
        }
      in
      loop env expr

    | Token.T_LBRACKET -> (* [] *)
      begin
        Eat.token env;
        let index_expr = parse_expression env in
        Expect.token env Token.T_RBRACKET;
        let spec = Index(expr, index_expr) in
        let expr =
          {
            spec;
            loc = with_start_loc env start_pos;
            attributes = [];
          }
        in
        loop env expr
      end

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
          attributes = [];
        }
      in
      loop env expr

    (* | Token.T_LCURLYBAR *)

    | _ -> expr

  in

  let expr = parse_primary_expression env in
  loop env expr

and parse_literal env =
  let start_loc = Peek.loc env in
  let next = Peek.token env in
  match next with
  | Token.T_NUMBER { raw; _ } ->
    Eat.token env;
    Literal.Integer (raw, None)

  | Token.T_STRING (loc, value, _, _) ->
    Eat.token env;
    Literal.String (value, loc, None)

  | Token.T_TRUE ->
    Eat.token env;
    Literal.Boolean true

  | Token.T_FALSE ->
    Eat.token env;
    Literal.Boolean false

  | _ ->
    let perr_spec = Parser_env.get_unexpected_error (Peek.token env) in
    let err = Parse_error.error {
      perr_spec;
      perr_loc = with_start_loc env start_loc;
    } in
    Parse_error.error err

and parse_primary_expression env : Expression.t =
  let start_loc = Peek.loc env in
  let next = Peek.token env in

  if next = Token.T_LPAREN then
    parse_group_expression env
  else
    let spec : Expression.spec =
      match next with 
      | Token.T_IDENTIFIER _ -> (
        let ident = parse_identifier env in
          Identifier ident
      )

      | Token.T_NUMBER _
      | Token.T_STRING _
      | Token.T_TRUE
      | Token.T_FALSE
        -> Constant(parse_literal env)

      | Token.T_IF -> If (parse_expression_if env)

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

      | Token.T_POUND ->
        begin
          Eat.token env;
          Expect.token env Token.T_LCURLY;
          let map_entries = ref [] in

          while (Peek.token env) <> Token.T_RCURLY do
            let start_loc = Peek.loc env in
            let key = parse_literal env in
            Expect.token env Token.T_COLON;
            let value = parse_expression env in
            let entry = { Ast.Expression.
              map_entry_key = key;
              map_entry_value = value;
              map_entry_loc = with_start_loc env start_loc;
            } in
            map_entries := entry::(!map_entries);
            if (Peek.token env) = Token.T_COMMA then (
              Eat.token env
            )
          done;

          Expect.token env Token.T_RCURLY;
          Map (List.rev !map_entries)
        end

      | Token.T_LCURLY ->
        let blk = parse_block env in
        Block blk

      | Token.T_MATCH -> (
        Eat.token env;
        let continue_parse_body match_expr =
          let relaxed_block = parse_relaxed_kv_block env in
          Parser_helper.cast_relaxed_block_into_match ~start_loc match_expr relaxed_block
        in
        match (Peek.token env) with
        | Token.T_IDENTIFIER _ -> (
          let id = parse_identifier env in
          let relaxed_block = parse_relaxed_kv_block env in
          (* init and match *)
          if (Peek.token env) = Token.T_LCURLY then (
            let init = Parser_helper.cast_relaxed_block_into_init ~start_loc ~init_name:id relaxed_block in
            continue_parse_body init
          ) else (
            let id_expr = {
              Expression.
              spec = Identifier id;
              loc = id.pident_loc;
              attributes = [];
            } in
            Parser_helper.cast_relaxed_block_into_match ~start_loc id_expr relaxed_block
          )
        )

        | _ ->
          let match_expr = parse_expression env in
          continue_parse_body match_expr
      )

      | Token.T_THIS ->
        Eat.token env;
        This

      | Token.T_SUPER ->
        Eat.token env;
        Super

      | _ ->
        let tok = Token.token_to_string next in
        let lex_error = Lichenscript_lex.Lex_error.Unexpected tok in
        let perr_spec = Parse_error.LexError lex_error in
        let err =
          { Parse_error.
            perr_loc = with_start_loc env start_loc;
            perr_spec;
          }
        in
        Parse_error.error err

    in

    {
      spec;
      loc = with_start_loc env start_loc;
      attributes = [];
    }

and parse_expression_if env =
  let open Expression in
  let start_loc = Peek.loc env in
  Expect.token env Token.T_IF;
  let if_test = parse_expression env in
  let if_consequent = parse_block env in
  let has_else = Eat.maybe env Token.T_ELSE in
  let if_alternative =
    if has_else then (
      let alt =
        if (Peek.token env) = Token.T_LCURLY then (
          If_alt_block (parse_block env)
        ) else (
          let else_if = parse_expression_if env in
          If_alt_if else_if
        )
      in
      Some alt
    ) else
      None
    in
  { Expression.
    if_test;
    if_consequent;
    if_alternative;
    if_loc = with_start_loc env start_loc;
  }

and parse_group_expression env =
  parse_maybe_arrow_function env

and parse_relaxed_kv_block env = 
  let has_case = ref false in
  let parse_releaxed_kv env =
    let start_loc = Peek.loc env in
    let open Parser_helper in
    match (Peek.token env) with
    | Token.T_ELLIPSIS -> (
      Eat.token env;
      let expr = parse_expression env in
      Relaxed_spread(start_loc, expr)
    )
    | _ -> (
      let relaxed_case_prefix =
        match (Peek.token env) with
        | Token.T_CASE ->
          Eat.token env;
          has_case := true;
          true;
        | _ -> false
      in
      let relaxed_key = parse_pattern env in
      let relaxed_op = Peek.token env in
      (match relaxed_op with
      | Token.T_COLON
      | Token.T_ARROW -> ()
      | _ -> (
        let perr_spec = Parser_env.get_unexpected_error (Peek.token env) in
        let err = Parse_error.error {
          perr_spec;
          perr_loc = with_start_loc env start_loc;
        } in
        Parse_error.error err
      ));
      Eat.token env;
      let relaxed_value = parse_expression env in
      Relaxed_node {
        relaxed_case_prefix;
        relaxed_key;
        relaxed_op;
        relaxed_value;
        relaxed_kv_loc = with_start_loc env start_loc;
      }
    )
  in
  let start_loc = Peek.loc env in
  let entries = ref [] in
  Expect.token env Token.T_LCURLY;

  while (Peek.token env) <> Token.T_RCURLY do
    let kv = parse_releaxed_kv env in
    entries := kv::!entries;
    if not (!has_case) && (Peek.token env) = Token.T_COMMA then (
      Eat.token env
    )
  done;

  Expect.token env Token.T_RCURLY;
  { Parser_helper.
    entries = List.rev !entries;
    loc = with_start_loc env start_loc;
  }

and parse_pattern env : Pattern.t =
  let open Pattern in
  let start_loc = Peek.loc env in
  let next = Peek.token env in
  let spec =
    match next with
    | Token.T_IDENTIFIER _ -> (
      let ident = parse_identifier env in
      if (Peek.token env) == Token.T_LPAREN then (
        Eat.token env;
        let content = parse_pattern env in
        Expect.token env Token.T_RPAREN;
        EnumCtor(ident, content)
      ) else
        Identifier ident
    )

    | _ -> (
      let perr_spec = Parser_env.get_unexpected_error next in
      let err = Parse_error.error {
        perr_spec;
        perr_loc = with_start_loc env start_loc;
      } in
      Parse_error.error err
    )
  in

  {
    spec;
    loc = with_start_loc env start_loc;
  }

and parse_type env : Type.t =
  parse_primary_type env

and parse_primary_type env =
  let open Type in
  let start_loc = Peek.loc env in
  match (Peek.token env) with
  | Token.T_LPAREN -> (
    let params = parse_params env in

    Expect.token env Token.T_ARROW;

    let return_ty = parse_type env in

    {
      spec = Ty_arrow (params, return_ty);
      loc = with_start_loc env start_loc;
    }
  )

  | _ -> (
    let id = parse_identifier env in
    let args = ref [] in

    if Peek.token env = Token.T_LESS_THAN then (
      Eat.token env;

      while Peek.token env <> Token.T_GREATER_THAN do
        let t = parse_type env in
        args := t::(!args);
        if Peek.token env <> Token.T_GREATER_THAN then (
          Expect.token env Token.T_COMMA;
        )
      done;

      Expect.token env Token.T_GREATER_THAN
    );

    let prefix = {
      spec = Ty_ctor (id, List.rev !args);
      loc = with_start_loc env start_loc;
    } in
    parse_type_maybe_array env start_loc prefix
  )

and parse_type_maybe_array env start_loc prev =
  let open Type in
  match (Peek.token env) with
  | Token.T_LBRACKET -> (
    Eat.token env;
    Expect.token env Token.T_RBRACKET;
    let prefix = {
      spec = Ty_array prev;
      loc = with_start_loc env start_loc;
    } in
    parse_type_maybe_array env start_loc prefix
  )

  | _ -> prev
