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
open Lichenscript_typing
open Lichenscript_ir
open Core_kernel
open Sourcemap

type t = {
  prog: Program.t;
  buffer: Buffer.t;
  sourcemap: sourcemap_generator;

  mutable scope: Scope.scope option;

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

let create ~prog () =
  let sourcemap = new sourcemap_generator in
  let buffer = Buffer.create 1024 in
  {
    prog;
    buffer;
    sourcemap;
    scope = None;
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

let[@warning "-unused-value-declaration"] with_scope env scope f =
  let prev_scope = env.scope in
  env.scope <- Some scope;
  let result = f env in
  env.scope <- prev_scope;
  result

let print_indents env =
  for _ = 0 to (env.indents - 1) do
    ps env "  "
  done

let rec transpile_declaration env (delcaration: Ir.Decl.t) =
  let open Ir.Decl in
  let { spec; _ } = delcaration in
  match spec with
  | Class cls ->
    transpile_class env cls

  | Func _fun ->
    transpile_function env _fun

  | Enum enum -> (
    let { enum_has_meta_id; enum_name; enum_original_name; enum_members; _ } = enum in
    if not enum_has_meta_id then (
      ps env (Format.asprintf "const %s = {\n" enum_name);
      with_indents env (fun env ->
        print_indents env;
        ps env "[unionSym]: 1,\n";
        print_indents env;
        ps env "name: \"";
        ps env enum_original_name;
        ps env "\",\n";
        print_indents env;
        ps env "members: [\n";
        with_indents env (fun env ->
          List.iter
            ~f:(fun (member_name, _) -> 
              print_indents env;
              ps env "\"";
              ps env member_name;
              ps env "\",\n"
            )
            enum_members;
        );
        print_indents env;
        ps env "]\n";
      );
      ps env "};\n"
    )
  )

  | LambdaDef _ -> failwith "unimplement:lambda"

  | EnumCtor enum_ctor -> (
    ps env "function ";
    ps env enum_ctor.enum_ctor_name;
    ps env "() {\n";
    ps env "  return [";
    ps env enum_ctor.enum_ctor_meta_name;
    ps env ", ";
    ps env (Int.to_string enum_ctor.enum_ctor_tag_id);
    if enum_ctor.enum_ctor_params_size > 0 then (
      ps env ", ";
      for i = 0 to (enum_ctor.enum_ctor_params_size - 1) do (
        ps env "arguments[";
        ps env (Int.to_string i);
        ps env "]"
      ) done
    );
    ps env "];\n";
    ps env "}\n";
  )

  | GlobalClassInit _

  | FuncDecl _ -> ()

and transpile_class env (cls: Ir.Decl._class) =
  ps env ("const " ^ cls.name ^ " = {\n");

  (match cls.init.class_ancester with
  | Some (Ir.SymLocal v) -> 
    ps env (Format.asprintf "  __proto__: %s,\n" v);
  | _ -> 
    ps env (Format.asprintf "  __proto__: LCC_Object,\n");
  );

  ps env (Format.asprintf "  [clsNameSym]: \"%s\",\n" cls.original_name);

  List.iter
    ~f:(fun { class_method_name; class_method_gen_name } ->
      ps env ("  " ^ class_method_name ^ ": " ^ class_method_gen_name ^ ",\n");
    )
    cls.init.class_methods;

  ps env "}\n"

and transpile_if env if_spec =
  let open Ir.Stmt in
  let { if_test; if_consequent; if_alternate } = if_spec in
  ps env "if (";
  transpile_expression env if_test;
  ps env ") {\n";
  with_indents env (fun env ->
    List.iter
      ~f:(fun stmt ->
        print_indents env;
        transpile_statement env stmt
      )
      if_consequent
  );
  match if_alternate with
  | Some (If_alt_if else_spec) -> (
    print_indents env;
    ps env "} else ";
    transpile_if env else_spec
  )
  | Some (If_alt_block stmts) -> (
    print_indents env;
    ps env "} else {\n";
    with_indents env (fun env ->
      List.iter
        ~f:(fun stmt ->
          print_indents env;
          transpile_statement env stmt
        )
        stmts
    );
    print_indents env;
    ps env "}\n";
  )
  | None ->
    print_indents env;
    ps env "}\n"

and transpile_statement env decl =
  let open Ir.Stmt in
  match decl with
  (* elimiate unused code *)
  | Expr (Ir.Expr.Temp _) 
  | Expr (Ir.Expr.Ident _) -> ()

  | Expr expr ->
    transpile_expression ~parent_expr:false env expr;
    ps env ";\n"

  | If if_spec -> transpile_if env if_spec

  | While (while_test, while_block) -> (
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

  | VarDecl names -> (
    let names_len = List.length names in
    if names_len >0 then (
      ps env "var ";
      List.iteri
        ~f:(fun index name ->
          ps env name;
          if index <> (names_len - 1) then (
            ps env ", ";
          )
        )
        names;
      ps env ";\n";
    )
  )

  | Break -> ps env "break;\n"
  | Continue -> ps env "continue;\n"
  (* | Debugger -> ps env "debugger;\n" *)

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

  | Retain _
  | Release _ -> ()

  | WithLabel(label, stmts) -> (
    ps env "\n";
    ps env label;
    ps env ":\n";
    print_indents env;
    ps env "do {\n";

    List.iter
      ~f:(fun stmt ->
        print_indents env;
        transpile_statement env stmt
      )
      stmts;

    print_indents env;
    ps env "} while (false);\n";
  )

  | Goto label ->
    ps env "break ";
    ps env label;
    ps env ";\n"


and transpile_symbol env sym =
  let open Ir in
  match sym with
  | SymLocal name -> ps env name

  | SymTemp id ->
    ps env "t[";
    ps env (Int.to_string id);
    ps env "]"

  | SymParam param_index ->
    ps env "arguments[";
    ps env (Int.to_string param_index);
    ps env "]"

  | SymLambda (_, name) ->
    ps env name

  | SymRet -> ps env "ret"

  | SymThis
  | SymLambdaThis
    -> ps env "this"

and transpile_expression ?(parent_expr=true) env expr =
  let open Ir.Expr in
  match expr with
  | Null -> ps env "undefined"
  | NewChar ch -> (
    ps env "String.fromCharCode(";
    ps env (Int.to_string ch);
    ps env ")"
  )
  | NewInt raw -> ps env raw
  | NewFloat raw ->
    ps env "Math.fround(";
    ps env raw;
    ps env ")"

  | NewString content ->
    let content = Ir.Utils.escape_string content in
    ps env "\"";
    ps env content;
    ps env "\"";

  | NewBoolean bl ->
    ps env (if bl then "true" else "false")

  | NewLambda lambda -> (
    let { lambda_decl; _ } = lambda in
    ps env "(";
    transpile_declaration env lambda_decl;
    ps env ").bind(this)"
  )

  | NewRef expr ->
    transpile_expression env expr

  | GetRef(_, original_name) ->
    ps env original_name

  | NewArray len -> (
    ps env "Array(";
    ps env (Int.to_string len);
    ps env ")"
  )

  | NewTuple exprs -> (
    ps env "[tupleSym, ";
    let exprs_len = List.length exprs in
    List.iteri
      ~f:(fun index expr ->
        transpile_expression env expr;
        if index < (exprs_len - 1) then (
          ps env ", "
        )
      )
      exprs;
    ps env "]"
  )

  | NewMap _ -> ps env "new Map()"

  | Not expr ->
    ps env "!";
    ps env "(";
    transpile_expression env expr;
    ps env ")"

  | TupleGetValue (expr, index) ->
    transpile_expression env expr;
    ps env "[";
    ps env (Int.to_string (index + 1));
    ps env "]"

  | ArrayGetValue(expr, index) ->
    transpile_expression env expr;
    ps env "[";
    transpile_expression env index;
    ps env "]"

  | ArraySetValue (expr, index, value) -> (
    transpile_expression env expr;
    ps env "[";
    transpile_expression env index;
    ps env "] = ";
    transpile_expression env value;
  )

  | I32Binary(Asttypes.BinaryOp.Plus, left, right) -> (
    ps env "i32_add(";
    transpile_expression env left;
    ps env ", ";
    transpile_expression env right;
    ps env ")"
  )

  | I32Binary(op, left, right) ->
    transpile_i32_binary env op left right

  | I32BitNot expr ->
    ps env "i32_bit_not(";
    transpile_expression env expr;
    ps env ")"

  | F32Binary(op, left, right) ->
    transpile_f32_binary env op left right

  | I64Binary _
  | F64Binary _ -> failwith "unimplemented binary2"

  | CallLambda(expr, params) -> (
    transpile_expression env expr;
    ps env "(";
    let params_len = List.length params in
    List.iteri
      ~f:(fun index param ->
        transpile_expression env param;
        if index <> (params_len - 1) then (
          ps env ", "
        )
      )
      params;
    ps env ")"
  )

  | Invoke(expr, name, params) -> (
    transpile_expression env expr;
    ps env ".";
    ps env name;
    ps env "(";
    let params_len = List.length params in
    List.iteri
      ~f:(fun index param ->
        transpile_expression env param;
        if index <> (params_len - 1) then (
          ps env ", "
        )
      )
      params;
    ps env ")"
  )

  | Assign (left, right) -> (
    if parent_expr then (
      ps env "("
    );
    transpile_expression env left;
    ps env " = ";
    transpile_expression env right;
    if parent_expr then (
      ps env ")"
    );
  )

  | Call(name, this_opt, params) -> (
    transpile_symbol env name;
    ps env ".call(";
    (match this_opt with
    | Some this -> transpile_expression env this
    | None -> ps env "undefined");
    let params_len = List.length params in
    if params_len > 0 then (
      ps env ", ";
      List.iteri
        ~f:(fun index param ->
          transpile_expression env param;
          if index <> (params_len - 1) then (
            ps env ", "
          )
        )
        params
    );
    ps env ")"
  )

  | InitCall(_, proto_name) -> (
    ps env "{ __proto__: ";
    transpile_symbol env proto_name;
    ps env " }"
  )

  | Ident name -> transpile_symbol env name

  | TagEqual (expr, tag) -> (
    transpile_expression env expr;
    ps env "[1]";
    ps env " === ";
    ps env (Int.to_string tag)
  )

  | UnionGet (expr, tag) -> (
    transpile_expression env expr;
    ps env "[";
    ps env (Int.to_string (tag + 2));
    ps env "]"
  )

  | Temp index -> (
    ps env "t[";
    ps env (Int.to_string index);
    ps env "]"
  )

  | TypeCast(expr, Primitives.PrimType.Char, Primitives.PrimType.Char) -> (
    transpile_expression env expr;
  )

  | TypeCast(expr, Primitives.PrimType.Char, I32)
  | TypeCast(expr, Primitives.PrimType.Char, F32) -> (
    ps env "(";
    transpile_expression env expr;
    ps env ").charCodeAt(0)"
  )

  | TypeCast(expr, Primitives.PrimType.Char, Boolean) -> (
    ps env "!!((";
    transpile_expression env expr;
    ps env ").charCodeAt(0))"
  )

  | TypeCast(expr, _, Primitives.PrimType.Char)
  | TypeCast(expr, _, Primitives.PrimType.I32) -> (
    ps env "(";

    ps env "(";
    transpile_expression env expr;
    ps env ") | 0";

    ps env ")"
  )

  | TypeCast(expr, _, Primitives.PrimType.F32) -> (
    ps env "Math.fround(";

    transpile_expression env expr;

    ps env ")"
  )

  | TypeCast(expr, _, Primitives.PrimType.Boolean) -> (
    ps env "!!(";

    transpile_expression env expr;

    ps env ")"
  )

  | IntValue expr -> transpile_expression env expr

  | GetField(expr, _, field_name) -> (
    transpile_expression env expr;
    ps env ".";
    ps env field_name
  )

  | RawGetField _ -> failwith "unrechable raw get field"

  | StringCmp (op, left, right) -> (
    transpile_expression env left;
    ps env " ";
    let open Asttypes.BinaryOp in
    ps env (match op with
    | Equal -> "==="
    | NotEqual -> "!=="
    | GreaterThan -> ">"
    | GreaterThanEqual -> ">="
    | LessThan -> "<"
    | LessThanEqual -> "<="
    | _ -> failwith "unrechable string cmp"
    );
    ps env " ";
    transpile_expression env right;
  )

  | StringEqUtf8 (expr, str) -> (
    transpile_expression env expr;
    ps env " === ";
    ps env "\"";
    (* escape str *)
    ps env str;
    ps env "\"";
  )

  | Retaining expr ->
    transpile_expression env expr

and transpile_i32_binary env op left right =
  let open Asttypes.BinaryOp in
  (match op with
  | Plus
  | Minus
  | Mult
  | Div
  | LShift
  | RShift
  | Mod
    -> (
      ps env (match op with
      | Plus -> "i32_add"
      | Minus -> "i32_sub"
      | Mult -> "i32_mult"
      | Div -> "i32_div"
      | LShift -> "i32_lshift"
      | RShift -> "i32_rshift"
      | Mod -> "i32_mod"
      | _ -> failwith "unreachable"
      );

      ps env "(";
      transpile_expression env left;
      ps env ", ";
      transpile_expression env right;
      ps env ")"
    )

  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | BitOr
  | Xor
  | BitAnd
  | And
  | Or
    -> (
      ps env "(";
      transpile_expression env left;
      ps env (match op with
      | Equal -> "==="
      | NotEqual -> "!=="
      | LessThan -> "<"
      | LessThanEqual -> "<="
      | GreaterThan -> ">"
      | GreaterThanEqual -> ">="
      | BitOr -> "|"
      | Xor -> "^"
      | BitAnd -> "&"
      | And -> "&&"
      | Or -> "||"
      | _ -> failwith "unrechable binary");
      transpile_expression env right;
      ps env ")"
    )
  )

and transpile_f32_binary env op left right =
  let open Asttypes.BinaryOp in
  (match op with
  | Plus
  | Minus
  | Mult
  | Div
    -> (
      ps env (match op with
      | Plus -> "f32_add"
      | Minus -> "f32_sub"
      | Mult -> "f32_mult"
      | Div -> "f32_div"
      | _ -> failwith "unreachable"
      );

      ps env "(";
      transpile_expression env left;
      ps env ", ";
      transpile_expression env right;
      ps env ")"
    )

  | LShift
  | RShift
  | Mod -> failwith "unrechable"

  | Equal
  | NotEqual
  | LessThan
  | LessThanEqual
  | GreaterThan
  | GreaterThanEqual
  | BitOr
  | Xor
  | BitAnd
  | And
  | Or
    -> (
      ps env "(";
      transpile_expression env left;
      ps env (match op with
      | Equal -> "==="
      | NotEqual -> "!=="
      | LessThan -> "<"
      | LessThanEqual -> "<="
      | GreaterThan -> ">"
      | GreaterThanEqual -> ">="
      | BitOr -> "|"
      | Xor -> "^"
      | BitAnd -> "&"
      | And -> "&&"
      | Or -> "||"
      | _ -> failwith "unrechable binary");
      transpile_expression env right;
      ps env ")"
    )
  )

and transpile_function env _fun =
  let open Ir.Func in
  ps env "function ";
  transpile_id env _fun.name;
  ps env "(";

  (* tranpile_function_params env _fun.header.params; *)

  ps env ") {\n";
  if _fun.tmp_vars_count > 0 then (
    ps env ("  var t = Array(" ^ (Int.to_string _fun.tmp_vars_count) ^ ");\n")
  );
  transpile_function_body env _fun.body;
  ps env "}\n"

(* and tranpile_function_params env (params: Function.params) = 
  let open Function in
  let params_len = List.length params.params_content in
  List.iteri
    ~f:(fun index param ->
      let name = param.param_name in
      transpile_id env name param.param_loc;
      if index <> (params_len - 1) then (
        ps env ", "
      )
    )
    params.params_content; *)

and transpile_function_body env (body: Ir.Block.t) =
  let body_len = List.length body.body in
  with_indents env (fun env ->
    List.iteri
      ~f:(fun index stmt ->
        let is_last = index = (body_len - 1) in
        if is_last then (
          match stmt with
          | Expr expr -> (
            print_indents env;
            ps env "return ";
            transpile_expression env expr;
            ps env ";\n"
          )

          | _ -> (
            print_indents env;
            transpile_statement env stmt
          )
        ) else (
          print_indents env;
          transpile_statement env stmt
        )
      )
      body.body;
  );

and transpile_id env (name, loc) =
  env.sourcemap#add_location env.col 0 loc.start.line loc.start.column;
  ps env name

let transpile_program ~prog ~preclude ~init_calls declarations =
  let env = create ~prog () in
  ps env preclude;

  let transform_config = { Transform.
    arc = false;
    prepend_lambda = false;
  } in
  let ir_tree = Transform.transform_declarations ~config:transform_config prog declarations in

  List.iter ~f:(transpile_declaration env) ir_tree.declarations;

  List.iter
  ~f:(fun fun_name ->
    ps env fun_name;
    ps env "();\n";
  )
  init_calls;

  ps env "LCC_main();\n";
  env

let contents env = Buffer.contents env.buffer
