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
open Lichenscript_parsing
open Lichenscript_parsing.Asttypes
open Lichenscript_typing
open Lichenscript_typing.Typedtree
open Core_kernel
open Sourcemap

type t = {
  ctx: Type_context.t;
  buffer: Buffer.t;
  sourcemap: sourcemap_generator;

  global_name_map: (int, string) Hashtbl.t;

  mutable indents: int;
  mutable line: int;
  mutable col: int;
}

let ps env str =
  let lines = String.split str ~on:'\n' in
  List.iteri
    ~f:(fun index line ->
      Buffer.add_string env.buffer line;
      env.col <- env.col + (String.length line);
      if index >= 1 then (
        env.line <- env.line + 1;
        env.col <- 0;
        Buffer.add_string env.buffer "\n";
      );
    )
    lines

let create ~ctx () =
  let sourcemap = new sourcemap_generator in
  let buffer = Buffer.create 1024 in
  let global_name_map = Hashtbl.create (module Int) in
  {
    ctx;
    buffer;
    sourcemap;
    global_name_map;
    indents = 0;
    line = 1;
    col = 0;
  }

let with_indents env f =
  let prev = env.indents in
  env.indents <- env.indents + 1;
  let result = f env in
  env.indents <- prev;
  result

let print_indents env =
  for _ = 0 to (env.indents - 1) do
    ps env "  "
  done

let rec transpile_declaration env delcaration =
  let open Declaration in
  let { spec; _ } = delcaration in
  match spec with
  | Class cls ->
    transpile_class env cls

  | Function_ _fun ->
    transpile_function env _fun

  | Interface _
  | Declare _
  | Enum _
  | Import _ -> ()

and transpile_class env cls =
  let original_name, name_id = cls.cls_id in

  let given_name = "LCC_" ^ original_name in
  Hashtbl.set env.global_name_map ~key:name_id ~data:given_name;

  let transpile_class_method ~given_name _method =
    let { Typedtree.Declaration. cls_method_body; _ } = _method in
    ps env "function ";
    ps env given_name;
    ps env "() {\n";
    with_indents env (fun env ->
      List.iter
        ~f:(fun stmt ->
          print_indents env;
          transpile_statement env stmt;
        )
        cls_method_body.body
    );
    ps env "}\n"
  in

  let names = 
    List.filter_map
      ~f:(fun elm ->
        match elm with
        | Cls_method _method -> (
          let { Typedtree.Declaration. cls_method_name = method_name', method_id; _ } = _method in
          let method_name = "LCC_" ^ original_name ^ "_" ^ method_name' in
          Hashtbl.set env.global_name_map ~key:method_id ~data:method_name;

          transpile_class_method ~given_name:method_name _method;

          Some (method_name', method_name)
        )

        | _ -> None
      )
      cls.cls_body.cls_body_elements
  in

  ps env ("const " ^ given_name ^ " = {\n");

  List.iter
    ~f:(fun (original_name, name) ->
      ps env ("  " ^ original_name ^ ": " ^ name ^ ",\n");
    )
    names;

  ps env "}\n"

and transpile_statement env decl =
  let open Statement in
  match decl.spec with
  | Expr expr -> transpile_expression env expr
  | Semi expr ->
    transpile_expression env expr;
    ps env ";\n"

  | While { while_test; while_block; _ } -> (
    ps env "while (";
    transpile_expression env while_test;
    ps env ") {\n";

    with_indents env (fun env ->
      List.iter
      ~f:(fun stmt ->
        print_indents env;
        transpile_statement env stmt
      )
      while_block.body;
    );

    print_indents env;
    ps env "}\n"
  )

  | Binding { binding_kind; binding_pat; binding_init; _ } -> (
    ps env (match binding_kind with
    | Ast.Pvar_let -> "let"
    | Ast.Pvar_const -> "const"
    );
    
    ps env " ";

    ps env (match binding_pat.spec with
    | Pattern.Symbol (name, _) -> name
    | _ -> failwith "unrechable"
    );

    ps env " = ";

    transpile_expression env binding_init;

    ps env ";\n";
  )

  | Break _ -> ps env "break;\n"
  | Continue _ -> ps env "continue;\n"
  | Debugger -> ps env "debugger;\n"

  | Return expr_opt -> (
    ps env "return";
    (match expr_opt with
    | Some expr -> (
      ps env " ";
      transpile_expression env expr;
    )
    | None -> ());
    ps env ";\n"
  )

  | Empty -> ps env ";\n"

and transpile_expression env expr =
  let open Expression in
  match expr.spec with
  | Constant (Ast.Literal.Unit) -> ps env "undefined"
  | Constant (Ast.Literal.Char ch) -> (
    ps env "'";
    ps env (Char.to_string ch);
    ps env "'"
  )
  | Constant (Ast.Literal.Integer (raw, _)) -> ps env raw
  | Constant (Ast.Literal.Float (raw, _)) -> ps env raw
  | Constant (Ast.Literal.String(content, _, _)) ->
    ps env "\"";
    ps env content;
    ps env "\"";

  | Constant (Ast.Literal.Boolean bl) ->
    ps env (if bl then "true" else "false")

  | Identifier (name, _) -> ps env name
  | Lambda _ -> ()
  | If { if_test; if_consequent; if_alternative; _ } -> (
    ps env "if (";
    transpile_expression env if_test;
    ps env ") {\n";

    List.iter
      ~f:(fun stmt ->
        print_indents env;
        transpile_statement env stmt;
      )
      if_consequent.body;

    match if_alternative with
    | _ -> ps env "}\n";
  )

  | Array elements -> (
    let len = List.length elements in
    ps env "[";
    List.iteri
      ~f:(fun index elm ->
        transpile_expression env elm;
        if index <> (len - 1) then (
          ps env ", "
        )
      )
      elements;
    ps env "]";
  )

  | Map _ -> ()

  | Call { callee; call_params; _} -> (
    match callee with
    | { spec = Identifier (_, id); _ } -> (
      (match Type_context.find_external_symbol env.ctx id with
      | Some ext_name -> (
        (* external method *)
        ps env ext_name;
      )

      | _ -> (
        let given_name = Hashtbl.find_exn env.global_name_map id in
        ps env given_name
      ));

      ps env "(";

      let params_len = List.length call_params in
      List.iteri
        ~f:(fun index item ->
          transpile_expression env item;
          if index <> (params_len - 1) then (
            ps env ", "
          )
        )
        call_params;

      ps env ")"

    )

    | _ -> ps env "/* unimplemented */{}"
  )

  | Tuple elements -> (
    let len = List.length elements in
    ps env "[";
    List.iteri
      ~f:(fun index elm ->
        transpile_expression env elm;
        if index <> (len - 1) then (
          ps env ", "
        )
      )
      elements;
    ps env "]";
  )

  | Member (expr, name) -> (
    transpile_expression env expr;
    ps env ".";
    ps env name.pident_name
  )

  | Index _
  | Unary _ -> ()
  | Binary (op, left, right) -> (
    transpile_expression env left;
    let open BinaryOp in
    ps env (match op with
    | Equal -> "==="
    | NotEqual -> "!=="
    | LessThan -> "<"
    | LessThanEqual -> "<="
    | GreaterThan -> ">"
    | GreaterThanEqual -> ">="
    | LShift -> "<<"
    | RShift -> ">>"
    | Plus -> "+"
    | Minus -> "-"
    | Mult -> "*"
    | Div -> "/"
    | Mod -> "%"
    | BitOr -> "|"
    | Xor -> "^"
    | BitAnd -> "&"
    );
    transpile_expression env right
  )

  | Assign (op, left, right) -> (
    transpile_expression env left;
    let open AssignOp in
    ps env " ";
    ps env (match op with
    | None -> "="
    | Some PlusAssign -> "+="
    | Some MinusAssign -> "-="
    | Some MultAssign -> "*="
    | Some DivAssign -> "/="
    | Some ModAssign -> "%="
    | Some LShiftAssign -> "<<="
    | Some RShiftAssign -> ">>="
    | Some BitOrAssign -> "|="
    | Some BitXorAssign -> "^="
    | Some BitAndAssign -> "&="
    );
    ps env " ";
    transpile_expression env right
  )

  | Block _ -> ()

  | Init init -> (
    let { Typedtree.Expression. init_name = (_, name_id); _ } = init in
    let given_name = Hashtbl.find_exn env.global_name_map name_id in
    ps env "{\n";
    with_indents env (fun env ->
      print_indents env;
      ps env "__proto__: ";
      ps env given_name;
      ps env ",\n";
    ());
    print_indents env;
    ps env "}\n"
  )

  | Match _ -> ()
  | This -> ps env "this"
  | Super -> ()

and transpile_function env _fun =
  let open Function in
  ps env "function ";
  tranpile_id env _fun.header.name _fun.header.name_loc;
  ps env "() {\n";
  with_indents env (fun env ->
    List.iter
      ~f:(fun stmt ->
        print_indents env;
        transpile_statement env stmt
      )
      _fun.body.body;
  );
  ps env "}\n"

and tranpile_id env (name, _) (loc: Loc.t) =
  env.sourcemap#add_location env.col 0 loc.start.line loc.start.column;
  ps env name

let transpile_program ~ctx ~preclude declarations =
  let env = create ~ctx () in
  ps env preclude;
  List.iter ~f:(transpile_declaration env) declarations;
  ps env "main();\n";
  env

let contents env = Buffer.contents env.buffer
