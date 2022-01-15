open Lichenscript_typing.Typedtree
open Lichenscript_codegen_utils
open Lichenscript_typing
open Lichenscript_parsing
open Core_kernel

let main_snippet ?init_name main_name = {|
int main() {
  int ec = 0;
  LCValue ev;
  LCRuntime*rt = LCNewRuntime();
  |} ^ Option.value ~default:"" (Option.map ~f:(Format.sprintf "%s(rt);") init_name) ^ {|
  LCProgram program = { rt, |} ^ main_name ^ {| };
  ev = LCRunMain(&program);
  ec = ev.int_val;
  LCFreeRuntime(rt);
  
  return ec;
}
|}

type cls_init =
  | InitClass of (string * string)
  | InitMethods of (string * string)

type cls_method_entry = {
  cls_method_origin_name: string;
  cls_method_gen_name: string;
}

type t = {
  indent: string;
  ctx: Type_context.t;
  mutable indent_level: int;
  mutable buffer: Buffer.t;
  mutable statements: string list;
  mutable scope: Codegen_scope.scope;
  mutable cls_inits: cls_init list;
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

let create ?(indent="    ") ~ctx () =
  let preserve_names = ["main"] in
  let scope = new Codegen_scope.scope ~preserve_names () in
  {
    ctx;
    indent;
    indent_level = 0;
    buffer = Buffer.create 1024;
    statements = [];
    scope;
    cls_inits = [];
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
  | Class cls -> codegen_class env cls
  | Function_ _fun -> codegen_function env _fun
  | Declare _ -> ()
  | Enum _ -> ()
  | Import _ -> ()

and codegen_class env _class =
  let open Declaration in
  let { cls_id; cls_body; _ } = _class in
  let name = (match cls_id with
  | (name, _) -> name
  ) in

  let class_id_var_name = name ^ "_class_id" in
  ps env (Format.sprintf "static LCClassID %s;\n" class_id_var_name);

  (* gen properties *)
  ps env (Format.sprintf "typedef struct %s {" name);
  endl env;

  with_indent env (fun () -> 
    print_indents env;
    ps env "LC_OBJ_HEADER";
    endl env;

    List.iter
      ~f:(fun elm ->
        match elm with
        | Cls_method _ -> ()
        | Cls_property prop -> (
          let { cls_property_name; _ } = prop in
          print_indents env;
          ps env (Format.sprintf "LCValue %s;" cls_property_name.pident_name);
          endl env;
        )
      )
      cls_body.cls_body_elements;
  );

  ps env (Format.sprintf "} %s;" name);
  endl env;

  ps env (Format.sprintf "LCValue %s_init(LCRuntime* rt, LCValue ancester) {\n" name);
  with_indent env (fun () ->
    print_indents env;
    ps env (Format.sprintf "%s* obj = lc_mallocz(rt, sizeof(%s));\n" name name);
    print_indents env;
    ps env (Format.sprintf "lc_init_object(rt, %s_class_id, (LCObject*)obj);\n" name);
    print_indents env;
    ps env "return MK_CLASS_OBJ(obj);\n";
  );
  ps env "}\n";

  let methods = ref [] in

  (* gen methods *)
  List.iter
    ~f:(fun elm ->
      match elm with
      | Cls_property _ -> ()
      | Cls_method _method -> (
        let method_name, _ = _method.cls_method_name in
        let gen_name = Format.sprintf "%s__%s" name method_name in
        ps env (Format.sprintf "LCValue %s(LCRuntime* rt, LCValue this, int arg_len, LCValue* args) {\n" gen_name);
        ps env "    LCValue ret = MK_NULL();\n";
        let stmts = codegen_function_block env (Option.value_exn _method.cls_method_body) in
        let max_tmp_value = List.fold ~init:0
          ~f:(fun acc item -> if item.tmp_vars_counter > acc then item.tmp_vars_counter else acc)
          stmts
        in

        if max_tmp_value > 0 then (
          print_indents env;
          ps env "LCValue t[";
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
        ps env "    return ret;\n";
        ps env "}\n";

        methods := { cls_method_origin_name = method_name; cls_method_gen_name = gen_name }::(!methods)
      )
    )
    cls_body.cls_body_elements;

  ps env (Format.sprintf "void %s_finalizer(LCRuntime* rt, LCValue value) {\n" name);
  (* with_indent env (fun () ->
    print_indents env;
    ps env (Format.sprintf "%s* obj = lc_mallocz(rt, sizeof(%s));\n" name name);
    print_indents env;
    ps env "obj->header.count = 1;\n";
    print_indents env;
    ps env "return MK_CLASS_OBJ(obj);\n";
  ); *)
  ps env "}\n";

  let class_def_name = name ^ "_def" in
  ps env (Format.sprintf "static LCClassDef %s = {\n" class_def_name);
  with_indent env (fun () ->
    print_indents env;
    ps env (Format.sprintf "\"%s\",\n" name);
    print_indents env;
    ps env "NULL,";
    endl env;
    print_indents env;
    ps env (Format.sprintf "%s_finalizer,\n" name);
  );
  ps env "};\n";

  env.cls_inits <- (InitClass (class_id_var_name, class_def_name))::env.cls_inits;

  if not (List.is_empty !methods) then (
    let method_def_name = name ^ "_methods" in
    ps env (Format.sprintf "static LCClassMethodDef %s[] = {\n" method_def_name);

    (!methods
    |> List.rev
    |> List.iter
    ~f:(fun entry ->
      ps env (Format.sprintf "    {\"%s\", 0, %s},\n" entry.cls_method_origin_name entry.cls_method_gen_name)
    ));

    ps env "};\n";

    env.cls_inits <- (InitMethods (class_id_var_name, method_def_name))::env.cls_inits;
  );

  ()

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
    match callee.spec with
    | Expression.Identifier (sym_name, sym_id) -> (
      let ext_name_opt = Type_context.find_external_symbol env.env.ctx sym_id in
      match ext_name_opt with
      | Some ext_method_name -> (
        pss env ext_method_name;
        let params_len = List.length call_params in
        let params_len_m1 = params_len - 1 in
        pss env ("(rt, MK_NULL(), " ^ (Int.to_string params_len) ^ ", (LCValue[]){ ");
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
    | Expression.Member (expr, name) -> (
      (* let ty_node = Type_context.get_node env.env.ctx expr.ty_var in *)
      let ctor = Check_helper.find_construct_of env.env.ctx expr.ty_var in
      match ctor with
      | Some (def, _) -> (
        (* let open Core_type.TypeExpr in *)
        match def.spec with
        | Class cls -> (
          let cls_name = cls.tcls_name in
          let static_method =
            List.find ~f:(fun (m_name, _) -> String.equal m_name name.pident_name) cls.tcls_static_elements
          in
          match static_method with
          (* static method *)
          | Some _ -> (
            let method_name = cls_name ^ "__" ^ name.pident_name in
            pss env (Format.sprintf "%s(rt, MK_NULL(), 0, NULL);" method_name)
          )
          | None -> (
            pss env (Format.sprintf "LCInvokeStr(rt, child, \"%s\", 0, NULL);\n" name.pident_name)
          )
        )
        
        | _ ->
          pss env "MK_NULL()"
      )
      | _ ->
        pss env (Format.sprintf "LCInvokeStr(rt, child, \"%s\", 0, NULL);\n" name.pident_name)
    )

    | _ ->
      pss env "MK_NULL()"

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

  | Assign((name, _), right) -> (
    pss env name;
    pss env " = ";
    codegen_expression env right;
    pss env ";"
  )

  | Init init -> (
    let { Ast.Expression. init_name; _ } = init in
    pss env (Format.sprintf "%s_init(rt, MK_NULL())" init_name.pident_name);
  )

  | Block  _ -> ()
  | Match _ -> ()

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

and codegen_identifier env id =
  ps env (env.scope#codegen_id id)

and codegen_function env (_fun: Typedtree.Function.t) =
  let open Function in
  (* let fun_node = Type_context.get_node env.ctx _fun.header.id in *)
  let fun_name = _fun.header.name in
  ps env "LCValue ";
  codegen_identifier env (fun_name, _fun.header.id);
  ps env "(LCRuntime* rt, LCValue this, int arg_len, LCValue* args)";
  ps env " {";
  endl env;

  with_indent env (fun () ->
    let { scope; body; _ } = _fun in
    let vars = scope#vars in
    let vars_len_m1 = (List.length vars) - 1 in

    print_indents env;
    ps env "LCValue ret = MK_NULL();";
    endl env;

    if vars_len_m1 >= 0 then (
      print_indents env;
      ps env "LCValue ";
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
      ps env "LCValue t[";
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

let codegen_program ?indent ~ctx (declarations: Typedtree.Declaration.t list) =
  let env = create ?indent ~ctx () in
  ps env {|/* This file is auto generated by LCc */
#include <stdint.h>
#include "runtime.h"
|};
  List.iter ~f:(codegen_declaration env) declarations;

  let init_name = if not (List.is_empty env.cls_inits) then (
    endl env;
    ps env "void init_class_meta(LCRuntime* rt) {\n";

    List.iter
      ~f:(fun entry ->
        match entry with
        | InitClass (id_name, gen_name) ->
          ps env (Format.sprintf "    %s = LCDefineClass(rt, &%s);\n" id_name gen_name)
        | InitMethods (id_name, cls_def_name) ->
          ps env (Format.sprintf "    LCDefineClassMethod(rt, %s, %s, countof(%s));\n" id_name cls_def_name cls_def_name)
      )
      (List.rev env.cls_inits);

    ps env "}\n";
    Some "init_class_meta"
  ) else None in

  (* if user has a main function *)
  let main_name = env.scope#id_name_exn "main" in
  ps env (main_snippet ?init_name main_name);

  env
