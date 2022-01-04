open Core_kernel
open Waterlang_typing
open Waterlang_typing.Typedtree

type t = {
  indent: string;
  mutable indent_level: int;
  buffer: Buffer.t;
  mutable tmp_vars_counter: int;
}

let create ?(indent="    ") () =
  {
    indent;
    indent_level = 0;
    buffer = Buffer.create 1024;
    tmp_vars_counter = 0;
  }

let ps env content = Buffer.add_string env.buffer content

let endl env = ps env "\n"

let print_indents env =
  let count = ref 0 in
  while !count < env.indent_level do
    ps env env.indent;
    count := !count + 1
  done

let with_indent env cb =
  let prev_indent = env.indent_level in
  env.indent_level <- prev_indent + 1;
  let result  = cb () in
  env.indent_level <- prev_indent;
  result

let rec codegen_statement env stmt =
  let open Statement in
  let { spec; _ } = stmt in
  match spec with
  | Class _
  | Module _ -> ()
  | Expr expr -> codegen_expression env expr
  | Semi expr -> (
    codegen_expression env expr;
    ps env ";"
  )
  | Function_ fun_ -> codegen_function env fun_
  | While _ -> ()

  | Binding binding -> (
    let { binding_pat; binding_init; _ } = binding in
    codegen_pattern env binding_pat;
    ps env " = ";
    codegen_expression env binding_init;
    ps env ";"
  )

  | Block _
  | Break _
  | Continue _
  | Debugger -> ()

  | Return expr_opt -> (
    ps env "return";
    (match expr_opt with
    | Some expr -> (
      ps env " ";
      codegen_expression env expr
    )

    | None -> ()
    );
    ps env ";"
  )

  | EnumDecl _ -> ()
  | Decl _
  | Empty -> ()

and codegen_program env (program: Typedtree.program) =
  let { tprogram_statements; _ } = program in
  List.iter ~f:(codegen_statement env) tprogram_statements

and codegen_expression env (expr: Typedtree.Expression.t) =
  let open Expression in
  let { spec; _ } = expr in
  match spec with
  | Constant cnst -> (
    let open Waterlang_parsing.Ast.Literal in
    match cnst with
    | Integer (raw, _) -> (
      ps env Primitives.Value.mk_i32;
      ps env "(";
      ps env raw;
      ps env ")"
    )

    | Char ch -> (
      ps env "'";
      ps env (Char.to_string ch);
      ps env "'";
    )

    | String (str, _, _) -> (
      let len = String.length str in
      ps env Primitives.Value.new_string_len;
      ps env "(rt, ";
      ps env "\"";
      ps env str;
      ps env "\", ";
      ps env (Int.to_string len);
      ps env ")"
    )

    | Float (str, _) -> (
      ps env Primitives.Value.mk_f32;
      ps env "(";
      ps env str;
      ps env ")"
    )

    | Boolean true ->
      ps env Primitives.Constant._true

    | Boolean false ->
      ps env Primitives.Constant._false
  )

  | Identifier sym -> (
    let name = Core_type.VarSym.name sym in
    ps env name
  )

  | Lambda
  | If _
  | Array _ -> ()

  | Call call -> (
    let { callee; call_params; _ } = call in
    let { callee_spec; _ } = callee in
    match callee_spec with
    | sym, [] -> (
      match sym.spec with
      | Core_type.VarSym.ExternalMethod ext_method_name -> (
        ps env ext_method_name;
        ps env "(";
        let params_len_m1 = (List.length call_params) - 1 in
        List.iteri
          ~f:(fun index item ->
            codegen_expression env item;
            if index <> params_len_m1 then (
              ps env ", "
            )
          )
          call_params;
        ps env ")"
      )
      | _ ->
      failwith "not implemented"
    )
    | _ ->
      failwith "not implemented"

  )

  | Member _
  | Unary _ -> ()

  | Binary (op, left_id, right_id) -> (
    let op_name = Primitives.Bin.prim op in
    ps env op_name;
    ps env "(";
    codegen_expression env left_id;
    ps env ", ";
    codegen_expression env right_id;
    ps env ")"
  )

  | Update _ -> ()

  | Assign(left, right) -> (
    codegen_pattern env left;
    ps env " = ";
    codegen_expression env right;
    ps env ";"
  )

  | Block  _ -> ()

and codegen_function_block (env: t) block =
  let open Block in
  let { body; _ } = block in
  let max_tmp_val = ref 0 in
  List.iter
    ~f:(fun stmt ->
      env.tmp_vars_counter <- 0;
      print_indents env;
      codegen_statement env stmt;
      endl env;

      if env.tmp_vars_counter > !max_tmp_val then (
        max_tmp_val := env.tmp_vars_counter
      )
    )
    body;
  !max_tmp_val

and codegen_function env (_fun: Typedtree.Function.t) =
  let open Function in
  let codegen_function_body body =
    match body with
    | Fun_block_body block -> (
      codegen_function_block env block
    )
    | Fun_expression_body _ ->
      failwith "not implemented"
  in
  let fun_name = Core_type.VarSym.name _fun.header.id in
  ps env "WTValue ";
  ps env fun_name;
  ps env "(WTRuntime* rt, WTValue* args, uint32_t arg_len)";
  ps env " {";
  endl env;

  with_indent env (fun () ->
    print_indents env;
    ps env "WTValue ";
    let { assoc_scope; body; _ } = _fun in
    let vars = Scope.vars assoc_scope in
    let vars_len_m1 = (List.length vars) - 1 in

    List.iteri
      ~f:(fun index (name, _item) ->
        ps env name;
        if index <> vars_len_m1 then
          ps env ", "
        else
          ps env ";"
      )
      vars;

    endl env;

    ignore (codegen_function_body body);
  );

  ps env "}"

and codegen_pattern env pat =
  let open Pattern in
  let { spec; _ } = pat in
  match spec with
  | Symbol sym ->
    ps env (Core_type.VarSym.name sym)

let contents env = Buffer.contents env.buffer
