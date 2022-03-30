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
  type 'a arrow ={
    arrow_content: 'a;
    arrow_loc: Loc.t
  }

  type 'a param = {
    param_content: 'a;
    param_colon: (Type.t * Loc.t) option;
    param_loc: Loc.t
  }

  type 'a t = {
    params: 'a param list;
    params_loc: Loc.t;
    return_ty: Type.t option;
    arrow: 'a arrow option;
    loc: Loc.t;
  }
  
end

let with_start_loc env start_loc =
  let loc = last_loc env in
  match loc with
  | Some loc -> Loc.btwn start_loc loc
  | None -> start_loc

let parse_relaxed_arrow env ~(f: env -> 'a) parse_type : 'a ReleaxedArrow.t =
  let start_pos = Peek.loc env in

  let parse_relaxed_param () =
    let start_pos = Peek.loc env in
    let param_content = Parser_env.with_allow_init env true f in
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
      param_content;
      param_colon;
      param_loc = with_start_loc env start_pos;
    }
  in

  let params = ref [] in

  Parser_env.with_allow_arrow env true (fun env ->
    Expect.token env Token.T_LPAREN;

    while (Peek.token env) <> Token.T_RPAREN do
      let param = parse_relaxed_param () in
      if (Peek.token env) = Token.T_COMMA then (
        Eat.token env
      );
      params := param::!params
    done;

    Expect.token env Token.T_RPAREN;
  );

  let params_loc = with_start_loc env start_pos in

  let return_ty =
    if (Peek.token env) = Token.T_COLON then (
      Eat.token env;
      Parser_env.with_allow_arrow env false (fun env ->
        Some (parse_type env)
      )
    ) else
      None
  in

  let arrow =
    if (Parser_env.allow_arrow env) && (Peek.token env) = Token.T_ARROW then (
      let start_pos = Peek.loc env in
      Eat.token env;
      let arrow_content = f env in
      let arrow = {
        ReleaxedArrow.
        arrow_content;
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

and parse_string source ?filter_platform content = 
  try
    let env = Parser_env.init_env source ?filter_platform content in
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

  with
  | Parse_error.Error errors ->
    Result.Error errors

and parse_program env : program =
  let decls = ref [] in
  let start_loc = Peek.loc env in

  while Peek.token env <> Token.T_EOF do
    let decl = parse_declaration env in
    decls := decl::(!decls)
  done;

  let pprogram_declarations = List.rev !decls in

  let pprogram_declarations =
    if has_platform_filter env then
      List.filter (filter_declaration env) pprogram_declarations
    else
      pprogram_declarations
  in

  {
    pprogram_top_level = Parser_env.get_top_level env;
    pprogram_declarations;
    pprogram_comments = [];
    pprogram_loc = with_start_loc env start_loc;
  }

and parse_import env : Import.t =
  let import_tail_with_spec spec =
    let perr_loc = Peek.loc env in
    let next = Peek.token env in
    match next with 
    | Token.T_STRING (loc, content, _, _) -> (
      Eat.token env;
      ignore (Eat.maybe env Token.T_SEMICOLON);
      { Import.
        spec;
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
  in

  Eat.token env;
  let next = Peek.token env in
  match next with
  | Token.T_MULT -> (
    Eat.token env;
    Expect.token env Token.T_FROM;
    import_tail_with_spec (Some ImportAll)
  )

  | Token.T_IDENTIFIER _ -> (
    let id = parse_identifier env in
    Expect.token env Token.T_FROM;
    import_tail_with_spec (Some (ImportNamespace id))
  )

  | Token.T_STRING _ ->
    import_tail_with_spec None

  | _ ->
    let perr_spec = Parser_env.get_unexpected_error next in
    let perr_loc = Peek.loc env in
    Parse_error.error {
      perr_spec;
      perr_loc;
    }
    (* import_tail_with_spec None *)

and parse_declaration env : Declaration.t =
  let open Declaration in
  let start_loc = Peek.loc env in
  let attributes = parse_attributes env in
  let next = Peek.token env in
  let spec: Declaration.spec =
    match next with
    | Token.T_IMPORT -> (
      let import = parse_import env in
      Import import
    )

    | _ -> (
      let visibility = parse_visibility env in
      let next = Peek.token env in
      match next with
      | Token.T_CLASS ->
        Class (parse_class ~visibility env)

      | Token.T_FUNCTION -> (
        let _fun = parse_function ~visibility env in

        let { Function. header = { id; _ }; _ } = _fun in
        let { Identifier. pident_name; pident_loc; _ } = id in

        Parser_env.add_top_level env ~name:pident_name ~loc:pident_loc ~visibility;

        Function_ _fun
      )

      | Token.T_INTERFACE -> (
        let intf = parse_interface env ~visibility in

        let { Identifier. pident_name; pident_loc; _ } = intf.intf_name in
        Parser_env.add_top_level env ~name:pident_name ~loc:pident_loc ~visibility;

        Interface intf
      )

      | Token.T_DECLARE ->
        begin
          Eat.token env;
          let function_header = parse_function_header env in
          let spec = Declaration.DeclFunction function_header in

          let { Identifier. pident_name; pident_loc; _ } = function_header.id in
          Parser_env.add_top_level env ~name:pident_name ~loc:pident_loc ~visibility;

          if Peek.token env = Token.T_SEMICOLON then (
            Eat.token env
          );

          Declaration.Declare {
            decl_visibility = visibility;
            decl_spec = spec;
            decl_loc = with_start_loc env start_loc;
          }
        end

      | Token.T_ENUM -> Enum (parse_enum env ~visibility)

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

and parse_enum env ~visibility =
  let parse_case env =
    let start_loc = Peek.loc env in
    Expect.token env Token.T_CASE;
    let case_name = parse_identifier env in
    let { Identifier. pident_name; pident_loc; _ } = case_name in
    Parser_env.add_top_level env ~name:pident_name ~loc:pident_loc ~visibility;
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

  let parse_method env : Declaration.class_method =
    let start_loc = Peek.loc env in
    let attributes =
      if (Peek.token env) = Token.T_AT then
        parse_attributes env
      else
        []
    in
    let cls_method_visibility = parse_visibility env in
    let cls_method_name = parse_identifier env in
    let params = parse_params env in
    let cls_method_return_ty =
      if Peek.token env = Token.T_COLON then (
        Eat.token env;
        Some (parse_type env)
      ) else
        None
    in
    let cls_method_body = parse_block env in
    { Declaration.
      cls_method_attributes = attributes;
      cls_method_modifier = None;  (* do NOT support modifier currently *)
      cls_method_visibility;
      cls_method_name;
      cls_method_params = params;
      cls_method_body;
      cls_method_loc = with_start_loc env start_loc;
      cls_method_return_ty;
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

  let elements = ref [] in
  Expect.token env Token.T_LCURLY;

  while (Peek.token env) <> Token.T_RCURLY && (Peek.token env) <> Token.T_EOF do
    let elm =
      match (Peek.token env) with 
      | Token.T_CASE -> Enum.Case (parse_case env)
      | _ -> Enum.Method (parse_method env)
    in
    elements := elm::(!elements);
  done;

  Expect.token env Token.T_RCURLY;

  Parser_env.add_top_level env ~name:(name.pident_name) ~loc:name.pident_loc ~visibility;

  { Enum.
    visibility;
    name;
    type_vars;
    elements = List.rev !elements;
    loc = with_start_loc env start_loc;
  }

and parse_visibility env = 
  match (Peek.token env) with
  | Token.T_PUBLIC ->
    Eat.token env;
    Some Pvisibility_public

  | Token.T_PROTECTED ->
    Eat.token env;
    Some Pvisibility_protected

  | Token.T_PRIVATE ->
    Eat.token env;
    Some Pvisibility_private

  | Token.T_INTERNAL ->
    Eat.token env;
    Some Pvisibility_internal

  | _ -> None

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

    | Token.T_FOR ->
      begin
        let start_loc = Peek.loc env in
        Eat.token env;
        let for_pat = parse_pattern env in
        Expect.token env T_IN;
        let for_expr = parse_expression env in
        let for_block = parse_block env in
        let for_loc = with_start_loc env start_loc in
        For {
          for_pat;
          for_expr;
          for_block;
          for_loc;
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
      if Peek.token env == Token.T_SEMICOLON then (
        Eat.token env;
        Semi expr
      ) else
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
  let type_vars =
    if (Peek.token env) = Token.T_LESS_THAN then
      parse_type_vars env
    else []
  in
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
    type_vars;
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
  Parser_env.add_top_level env ~name ~loc:id.pident_loc ~visibility;

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

      let cls_decl_method_modifier =
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
      let cls_decl_method_name, cls_decl_method_get_set =
        match (Peek.token env) with
        (* is a getter *)
        | Token.T_IDENTIFIER _ ->
          (parse_identifier env), cls_decl_method_get_set

        (* is a method called get *)
        | _ -> first_id, None
      in
      let type_vars =
        if (Peek.token env) = Token.T_LESS_THAN then
          parse_type_vars env
        else []
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
        cls_decl_method_modifier;
        cls_decl_method_name;
        cls_decl_method_type_vars = type_vars;
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

      let const_loc = Peek.loc env in
      let has_const =
        match Peek.token env with
        | Token.T_CONST ->
          Eat.token env;
          true
        | _ -> false

      in

      let id = parse_identifier env in

      if Peek.token env = Token.T_LPAREN then
        begin
          if has_const then (
              let lex_error = Lichenscript_lex.Lex_error.Unexpected (Token.value_of_token Token.T_CONST) in
              let perr_spec = Parse_error.LexError lex_error in
              let err =
                { Parse_error.
                  perr_loc = const_loc;
                  perr_spec;
                }
                in
              Parse_error.error err
          );
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
          match cls_method_modifier with
          | Some Ast.Declaration.Cls_modifier_static -> (
            let cls_static_prop_type =
              if Peek.token env = Token.T_COLON then (
                Eat.token env;
                let t = parse_type env in
                Some t
              ) else
                None
            in

            Expect.token env Token.T_ASSIGN;

            let cls_static_prop_init = parse_expression env in

            if (Peek.token env) = Token.T_SEMICOLON then (
              Eat.token env;
            );

            Cls_static_property {
              cls_static_prop_attributes = attributes;
              cls_static_prop_visibility = v;
              cls_static_prop_loc = with_start_loc env start_pos;
              cls_static_prop_const = has_const;
              cls_static_prop_name = id;
              cls_static_prop_type;
              cls_static_prop_init;
            }
          )

          | Some _ ->
            begin
              let tok = Token.value_of_token (Peek.token env) in
              let perr_loc = Peek.loc env in
              let lex_error = Lichenscript_lex.Lex_error.Unexpected tok in
              let perr_spec = Parse_error.LexError lex_error in
              let err =
                { Parse_error.
                  perr_loc;
                  perr_spec;
                }
                in
              Parse_error.error err
            end
          | None ->
            if has_const then (
                let lex_error = Lichenscript_lex.Lex_error.Unexpected (Token.value_of_token Token.T_CONST) in
                let perr_spec = Parse_error.LexError lex_error in
                let err =
                  { Parse_error.
                    perr_loc = const_loc;
                    perr_spec;
                  }
                  in
                Parse_error.error err
            );
            Expect.token env Token.T_COLON;
            let cls_prop_type = parse_type env in

            if (Peek.token env) = Token.T_SEMICOLON then (
              Eat.token env;
            );
            Cls_property {
              cls_prop_attributes = attributes;
              cls_prop_visibility = v;
              cls_prop_loc = with_start_loc env start_pos;
              cls_prop_name = id;
              cls_prop_type;
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
  let next = Peek.token env in
  if next = Token.T_AS then (
    Eat.token env;
    let t = parse_type env in
    {
      spec = As (expr, t);
      loc = with_start_loc env start_loc;
      attributes = [];
    }
  ) else if Parser_env.allow_init env && next = Token.T_LCURLY then (
    let parse_init_with_name init_name =
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
          init_name;
          init_elements = List.rev !init_elements;
          init_loc = with_start_loc env start_loc;
        }
      }
    in

    let rec try_cast_member_into_id_list acc (expr: Expression.t) =
      match expr.spec with
      | Expression.Identifier id ->
        Some (id::acc)

      | Expression.Member(expr, name) ->
        let next = name::acc in
        try_cast_member_into_id_list next expr

      | _ -> None

    in

    match expr.spec with
    | Expression.Identifier id -> (
        parse_init_with_name [id]
      )

    | Expression.Member _ -> (
      let test_expr = try_cast_member_into_id_list [] expr in
      match test_expr with
      | Some name_list -> parse_init_with_name name_list
      | _ -> expr

    )

    | _ -> expr

  ) else
    expr

and parse_assignment_expression env : Expression.t =
  let start_pos = Peek.loc env in
  let left = parse_binary_expression env in
  let next = Peek.token env in
  let op, ok =
    match next with
    | Token.T_ASSIGN -> None, true
    | Token.T_PLUS_ASSIGN -> (Some Asttypes.AssignOp.PlusAssign), true
    | Token.T_MINUS_ASSIGN -> (Some Asttypes.AssignOp.MinusAssign), true
    | Token.T_MULT_ASSIGN -> (Some Asttypes.AssignOp.MultAssign), true
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
  let relaxed_arrow: Expression.t ReleaxedArrow.t = parse_relaxed_arrow env ~f:parse_expression parse_type in
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
            match param.param_content.spec with
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
    let lambda_body = arrow.arrow_content in
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
    | [] -> (
      { Expression.
        spec = Constant (Literal.Unit);
        loc = relaxed_arrow.loc;
        attributes = [];
      }
    )
    | first::[] -> (
      (match first.param_colon with
      | Some (_, perr_loc) ->
        let tok = Token.value_of_token Token.T_COLON in
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

      first.param_content
    )

    | _ -> (
      let expressions = List.map (fun p -> ReleaxedArrow.(p.param_content)) relaxed_arrow.params in
      { Expression.
        spec = Tuple expressions;
        loc = relaxed_arrow.loc;
        attributes = [];
      }
    )

  )

and parse_binary_expression env : Expression.t =
  let open Expression in
  let rec parse_binary_enhance env left_expr left_token =
    let start_loc = Peek.loc env in
    let expr = parse_exponentiation_expression env in
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
        Eat.token env;
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

  let expr = parse_exponentiation_expression env in
  let next = Peek.token env in
  let prec = Precedence.binary_precedence next in
  if prec > 0 then
    begin
      Eat.token env;
      parse_binary_enhance env expr next
    end
  else
    expr

and parse_exponentiation_expression env =
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
  let expr = parse_left_handside_expression_allow_call env in
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

    | Token.T_PLING ->
      begin
        Eat.token env;
        let spec = Try expr in
        let expr =
          {
            spec;
            loc = with_start_loc env start_pos;
            attributes = [];
          }
        in
        loop env expr
      end

    (* | Token.T_LCURLYBAR *)

    | _ -> expr

  in

  let expr = parse_primary_expression env in
  loop env expr

and parse_literal env =
  let start_loc = Peek.loc env in
  let next = Peek.token env in
  match next with
  | Token.T_INTEGER { content; is_long; _ } -> (
    Eat.token env;
    if not is_long then
      Literal.I32 (Int32.of_string content)
    else
      Literal.I64 (Int64.of_string content)
  )

  | Token.T_FLOAT { content; is_f32 ;_ } -> (
    Eat.token env;
    Literal.Float (content, is_f32)
  )

  | Token.T_CHAR(_, ch, _) ->
    Eat.token env;
    Literal.Char ch

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

      | Token.T_INTEGER _
      | Token.T_FLOAT _
      | Token.T_STRING _
      | Token.T_CHAR _
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
        let match_expr = Parser_env.with_allow_init env false parse_expression in
        let match_clauses = ref [] in
        Expect.token env Token.T_LCURLY;

        while (Peek.token env) <> Token.T_RCURLY do
          Expect.token env Token.T_CASE;
          let pat = parse_pattern env in

          Expect.token env Token.T_ARROW;

          let clause_consequent = parse_expression env in

          (* if (Peek.token env) <> Token.T_RCURLY then (
            Expect.token env Token.T_COMMA;
          ); *)

          let clause = {
            Expression.
            clause_pat = pat;
            clause_consequent;
            clause_loc = with_start_loc env start_loc;
          } in
          match_clauses := clause::(!match_clauses);
        done;

        Expect.token env Token.T_RCURLY;
        Match {
          match_expr;
          match_clauses = List.rev !match_clauses;
          match_loc = with_start_loc env start_loc;
        }
      )

      | Token.T_THIS ->
        Eat.token env;
        This

      | Token.T_SUPER ->
        Eat.token env;
        Super

      | _ ->
        let tok = Token.value_of_token next in
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
  let if_test = Parser_env.with_allow_init env false parse_expression in
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

and parse_pattern env : Pattern.t =
  let open Pattern in
  let start_loc = Peek.loc env in

  let rec parse_array_pattern_continue elements =
    match (Peek.token env) with
    | Token.T_RBRACKET ->
      Eat.token env;
      Array { elements = List.rev elements; rest = None }

    | Token.T_ELLIPSIS -> (
      Eat.token env;
      let pat = parse_pattern env in
      Expect.token env Token.T_RBRACKET;
      Array { elements = List.rev elements; rest = Some pat }
    )

    | _ ->
      let pat = parse_pattern env in
      if (Peek.token env) <> Token.T_RBRACKET then (
        Expect.token env Token.T_COMMA;
      );
      parse_array_pattern_continue (pat::elements)

  in

  let rec parse_id_with_name acc =
    let next = Peek.token env in
    match next with
    | Token.T_PERIOD ->
      Eat.token env;
      let ident = parse_identifier env in
      let next_acc = ident::acc in
      parse_id_with_name next_acc

    | _ ->
      List.rev acc

  in

  let next = Peek.token env in
  let spec =
    match next with
    | Token.T_IDENTIFIER _ -> (
      let ident = parse_id_with_name [parse_identifier env] in
      if (Peek.token env) == Token.T_LPAREN then (
        Eat.token env;
        let content = parse_pattern env in
        Expect.token env Token.T_RPAREN;
        EnumCtor(ident, content)
      ) else
        Identifier ident
    )

    | Token.T_INTEGER _
    | Token.T_FLOAT _
    | Token.T_STRING _
    | Token.T_CHAR _
    | Token.T_TRUE 
    | Token.T_FALSE ->
      let l = parse_literal env in
      Literal l

    | Token.T_LBRACKET ->
      Eat.token env;
      parse_array_pattern_continue []

    | Token.T_LPAREN -> (
      Eat.token env;
      let children = ref [] in

      while (Peek.token env) <> Token.T_RPAREN do
        let child = parse_pattern env in
        children := child::(!children);
        if (Peek.token env) <> Token.T_RPAREN then (
          Expect.token env Token.T_COMMA
        )
      done;

      Expect.token env Token.T_RPAREN;

      match !children with
      | [] -> Literal (Literal.Unit)
      | _ ->
        Tuple (List.rev !children)
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
    let relaxed_arrow: Type.t ReleaxedArrow.t = parse_relaxed_arrow env ~f:parse_type parse_type in
    match relaxed_arrow.arrow with
    | Some arrow -> (
      let params_content =
        List.map
          (fun param -> 
            let open ReleaxedArrow in
            let param_name =
              match param.param_content.spec with
              | Type.Ty_ctor([id], []) ->
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
          relaxed_arrow.params
      in
      let params_loc = relaxed_arrow.params_loc in
      let params = { Function. params_content; params_loc } in
      {
        spec = Ty_arrow (params, arrow.arrow_content);
        loc = with_start_loc env start_loc;
      }
    )

    | None -> (
      let children =
        List.map
          (fun param ->
            let open ReleaxedArrow in
            param.param_content
          )
          relaxed_arrow.params
      in
      match children with
      | [] -> (
        let perr_spec = Parser_env.get_unexpected_error (Token.T_RPAREN) in
        Parse_error.error {
          perr_spec;
          perr_loc = (Peek.loc env);
        }
      )

      | one::[] -> one

      | _ ->
        {
          spec = Ty_tuple children;
          loc = with_start_loc env start_loc;
        }
    )
  )

  | _ -> (
    let ids = parse_id_namespace env [] in
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
      spec = Ty_ctor (ids, List.rev !args);
      loc = with_start_loc env start_loc;
    } in
    parse_type_maybe_array env start_loc prefix
  )

and parse_id_namespace env acc =
  let id = parse_identifier env in
  let next = id::acc in
  match (Peek.token env) with
  | Token.T_PERIOD ->
    Eat.token env;
    parse_id_namespace env next

  | _ ->
    List.rev next

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
