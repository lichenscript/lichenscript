open Core_kernel
open Waterlang_parsing
open Waterlang_typing
open Waterlang_typing.Typedtree

type t = {
  indent: string;
  mutable indent_level: int;
  buffer: Buffer.t;
}

let create ?(indent="    ") () =
  {
    indent;
    indent_level = 0;
    buffer = Buffer.create 1024;
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
  | Class _ -> ()
  | Expr expr -> codegen_expression env expr
  | Semi _ -> ()
  | Function_ fun_ -> codegen_function env fun_
  | While _
  | Binding _
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

  | EnumDecl _
  | Decl _
  | Empty -> ()

and codegen_program env (program: Typedtree.program) =
  let { tprogram_statements; _ } = program in
  List.iter ~f:(codegen_statement env) tprogram_statements

and codegen_expression env (expr: Typedtree.Expression.t) =
  let open Expression in
  let { spec; _ } = expr in
  match spec with
  | Constant _
  | Identifier _
  | Lambda
  | Throw _
  | If _
  | Array _
  | Call _
  | Member _
  | Unary _ -> ()

  | Binary (Asttypes.BinaryOp.Plus, { spec = Identifier left_sym; _ }, { spec = Identifier right_sym; _}) -> (
    let left_id_name = Core_type.VarSym.name left_sym in
    let right_id_name = Core_type.VarSym.name right_sym in
    ps env "WTL_ADD_I32(";
    ps env left_id_name;
    ps env ", ";
    ps env right_id_name;
    ps env ")"
  )

  | Binary _

  | Update _
  | Assign _
  | Block  _ -> ()

and codegen_function env (_fun: Typedtree.Function.t) =
  let open Function in
  let codegen_function_body body =
    match body with
    | Fun_block_body block -> (
      let open Block in
      let { body; _ } = block in
      List.iter
        ~f:(fun stmt ->
          print_indents env;
          codegen_statement env stmt;
          endl env
        )
        body;
    )
    | Fun_expression_body _ -> ()
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

    codegen_function_body body;

    endl env
  );

  ps env "}"

let contents env = Buffer.contents env.buffer
