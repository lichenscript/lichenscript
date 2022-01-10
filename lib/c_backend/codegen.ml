open Lichenscript_typing.Typedtree
open Lichenscript_codegen_utils
open Lichenscript_typing
open Core_kernel

let main_snippet main_name = {|
int main() {
  int ec = 0;
  WTValue ev;
  WTRuntime*rt = WTNewRuntime();
  WTProgram program = { rt, |} ^ main_name ^ {| };
  ev = WTRunMain(&program);
  ec = ev.int_val;
  WTFreeRuntime(rt);
  
  return ec;
}
|}

type t = {
  indent: string;
  ctx: Type_context.t;
  mutable indent_level: int;
  mutable buffer: Buffer.t;
  mutable statements: string list;
  mutable scope: Codegen_scope.scope;
}

type stmt_env = {
  env: t;
  stmt_buffer: Buffer.t;
  mutable stmt_prepend_lines: string list;
  mutable stmt_append_lines: string list;
  mutable tmp_vars_counter: int;
}

let create_stmt_env env = {
  env;
  stmt_buffer = Buffer.create 128;
  stmt_prepend_lines = [];
  stmt_append_lines = [];
  tmp_vars_counter = 0;
}

let create ?(indent="    ") ~ctx ~scope () =
  let preserve_names = ["main"] in
  let scope = new Codegen_scope.scope ~preserve_names scope in
  {
    ctx;
    indent;
    indent_level = 0;
    buffer = Buffer.create 1024;
    statements = [];
    scope;
  }

let ps env content = Buffer.add_string env.buffer content
let pss env content = Buffer.add_string env.stmt_buffer content

let endl env = ps env "\n"

let endl_s env = pss env "\n"

let print_indents env =
  let count = ref 0 in
  while !count < env.indent_level do
    ps env env.indent;
    count := !count + 1
  done

let print_indents_s env =
  let count = ref 0 in
  while !count < env.env.indent_level do
    pss env env.env.indent;
    count := !count + 1
  done

let with_indent env cb =
  let prev_indent = env.indent_level in
  env.indent_level <- prev_indent + 1;
  let result  = cb () in
  env.indent_level <- prev_indent;
  result

let rec codegen_statement (env: stmt_env) stmt =
  let open Statement in
  let { spec; _ } = stmt in
  match spec with
  | Expr expr -> (
    pss env "ret = ";
    codegen_expression env expr;
    pss env ";"
  )
  | Semi expr -> (
    codegen_expression env expr;
    pss env ";"
  )

  | While { while_test; while_block; _ } -> (
    pss env "while (";
    codegen_expression env while_test;
    pss env ".int_val";
    pss env ") {";
    endl_s env;
    with_indent env.env (fun () ->
      List.iter ~f:(codegen_statement env) while_block.body
    );
    endl_s env;
    print_indents_s env;
    pss env "}";
    endl_s env
  )

  | Binding binding -> (
    let { binding_pat; binding_init; _ } = binding in
    codegen_pattern env binding_pat;
    pss env " = ";
    codegen_expression env binding_init;
    pss env ";"
  )

  | Block _
  | Break _
  | Continue _
  | Debugger -> ()

  | Return expr_opt -> (
    pss env "ret = ";
    (match expr_opt with
    | Some expr -> (
      pss env " ";
      codegen_expression env expr
    )

    | None -> ()
    );
    pss env ";"
  )

  | Empty -> ()

and codegen_declaration env decl =
  let open Declaration in
  let { spec; _ } = decl in
  match spec with
  | Class _ -> ()
  | Function_ _fun -> codegen_function env _fun
  | Declare _ -> ()
  | Enum _ -> ()
  | Import _ -> ()

and codegen_expression (env: stmt_env) (expr: Typedtree.Expression.t) =
  let open Expression in
  let { spec; _ } = expr in
  match spec with
  | Constant cnst -> (
    let open Lichenscript_parsing.Ast.Literal in
    match cnst with
    | Integer (raw, _) -> (
      pss env Primitives.Value.mk_i32;
      pss env "(";
      pss env raw;
      pss env ")"
    )

    | Char ch -> (
      pss env "'";
      pss env (Char.to_string ch);
      pss env "'";
    )

    | String (str, _, _) -> (
      let id = env.tmp_vars_counter in
      env.tmp_vars_counter <- env.tmp_vars_counter + 1;

      let len = String.length str in
      let prepend_content = Format.sprintf "t[%d] = %s(rt, (const unsigned char*)\"%s\", %d);" id Primitives.Value.new_string_len str len in
      let append_content = Format.sprintf "%s(rt, t[%d]);" Primitives.Value.release id in

      env.stmt_prepend_lines <- prepend_content::(env.stmt_prepend_lines);
      env.stmt_append_lines <- append_content::(env.stmt_append_lines);

      pss env "t[";
      pss env (Int.to_string id);
      pss env "]";
    )

    | Float (str, _) -> (
      pss env Primitives.Value.mk_f32;
      pss env "(";
      pss env str;
      pss env ")"
    )

    | Boolean true ->
      pss env Primitives.Constant._true

    | Boolean false ->
      pss env Primitives.Constant._false
  )

  | Identifier (name, _) -> (
    pss env name
  )

  | Lambda
  | If _
  | Array _ -> ()

  | Call call -> (
    let { callee; call_params; _ } = call in
    let { callee_spec; _ } = callee in
    match callee_spec with
    | (sym_name, sym_id), [] -> (
      let ext_name_opt = Type_context.find_external_symbol env.env.ctx sym_id in
      match ext_name_opt with
      | Some ext_method_name -> (
        pss env ext_method_name;
        let params_len = List.length call_params in
        let params_len_m1 = params_len - 1 in
        pss env ("(rt, MK_NULL(), " ^ (Int.to_string params_len) ^ ", (WTValue[]){ ");
        List.iteri
          ~f:(fun index item ->
            codegen_expression env item;
            if index <> params_len_m1 then (
              pss env ", "
            )
          )
          call_params;
        pss env "})"
      )
      | _ ->
      failwith (Format.sprintf "can not find external %s %d\n" sym_name sym_id)
    )
    | _ ->
      failwith "not implemented 3"

  )

  | Member _
  | Unary _ -> ()

  | Binary (op, left_id, right_id) -> (
    let op_name = Primitives.Bin.prim op in
    pss env op_name;
    pss env "(";
    codegen_expression env left_id;
    pss env ", ";
    codegen_expression env right_id;
    pss env ")"
  )

  | Update _ -> ()

  | Assign(left, right) -> (
    codegen_pattern env left;
    pss env " = ";
    codegen_expression env right;
    pss env ";"
  )

  | Block  _ -> ()

(* return the number of temp values *)
and codegen_function_block (env: t) block =
  let open Block in
  let { body; _ } = block in
  List.map
    ~f:(fun stmt ->
      let prev_buffer = env.buffer in
      env.buffer <- Buffer.create 128;
      
      let stmt_env = create_stmt_env env in
      codegen_statement stmt_env stmt;

      env.statements <- (Buffer.contents env.buffer)::env.statements;
      env.buffer <- prev_buffer;
      stmt_env
    )
    body

and codegen_identifier env name =
  ps env (env.scope#codegen_name name)

and codegen_function env (_fun: Typedtree.Function.t) =
  let open Function in
  (* let fun_node = Type_context.get_node env.ctx _fun.header.id in *)
  let fun_name = _fun.header.name in
  ps env "WTValue ";
  codegen_identifier env fun_name;
  ps env "(WTRuntime* rt, WTValue this, int arg_len, WTValue* args)";
  ps env " {";
  endl env;

  with_indent env (fun () ->
    let { scope; body; _ } = _fun in
    let vars = scope#vars in
    let vars_len_m1 = (List.length vars) - 1 in

    print_indents env;
    ps env "WTValue ret;";
    endl env;

    if vars_len_m1 >= 0 then (
      print_indents env;
      ps env "WTValue ";
      List.iteri
        ~f:(fun index (name, _item) ->
          ps env name;
          if index <> vars_len_m1 then
            ps env ", "
          else ()
        )
        vars;
      ps env ";";
      endl env;
    );

    let stmts = codegen_function_block env body in
    let max_tmp_value = List.fold ~init:0
      ~f:(fun acc item -> if item.tmp_vars_counter > acc then item.tmp_vars_counter else acc)
      stmts
    in

    if max_tmp_value > 0 then (
      print_indents env;
      ps env "WTValue t[";
      ps env (Int.to_string max_tmp_value);
      ps env "];";
      endl env;
    );

    List.iter
      ~f:(fun stmt_env ->
        List.iter
          ~f:(fun line ->
            print_indents env;
            ps env line;
            endl env
          )
          stmt_env.stmt_prepend_lines;
        let content = Buffer.contents stmt_env.stmt_buffer in
        print_indents env;
        ps env content;
        endl env;
        List.iter
          ~f:(fun line ->
            print_indents env;
            ps env line;
            endl env
          )
          stmt_env.stmt_append_lines;
      )
      stmts;

    print_indents env;
    ps env "return ret;";
    endl env;
  );

  ps env "}"

and codegen_pattern env pat =
  let open Pattern in
  let { spec; _ } = pat in
  match spec with
  | Symbol (name, _) ->
    pss env name

let contents env = Buffer.contents env.buffer

let codegen_program ?indent ~ctx (program: Typedtree.program) =
  let { tprogram_declarations; tprogram_scope; _ } = program in
  let env = create ?indent ~ctx ~scope:tprogram_scope () in
  ps env {|/* This file is auto generated by wtc */
#include <stdint.h>
#include "runtime.h"
|};
  List.iter ~f:(codegen_declaration env) tprogram_declarations;

  (* if user has a main function *)
  let test_main = tprogram_scope#find_var_symbol "main" in
  (match test_main with
  | Some _main_sym -> (
    (* let open Core_type.VarSym in
    match main_sym.def_type with
    | Core_type.TypeValue.Function _ -> *)
    let main_name = env.scope#codegen_name "main" in
      ps env (main_snippet main_name)
    (* | _ -> () *)
  )

  | None -> ()
  );
  env
