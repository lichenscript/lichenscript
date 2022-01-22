open Lichenscript_codegen_utils
open Lichenscript_typing
open C_op
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

(* type cls_init =
  | InitClass of (string * string)
  | InitMethods of (string * string) *)

(* type cls_method_entry = {
  cls_method_origin_name: string;
  cls_method_gen_name: string;
} *)

type t = {
  indent: string;
  ctx: Type_context.t;
  mutable indent_level: int;
  mutable buffer: Buffer.t;
  mutable statements: string list;
  mutable scope: Codegen_scope.scope;
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

let rec codegen_statement (env: t) stmt =
  let open Stmt in
  let { spec; _ } = stmt in
  match spec with
  | Expr expr ->
    codegen_expression env expr;
    ps env ";"

  | VarDecl names -> (
    ps env "LCValue ";
    let len_m1 = (List.length names) - 1 in
    List.iteri
      ~f:(fun index name ->
        ps env name;
        if index <> len_m1 then (
          ps env ", "
        )
      )
      names;
    ps env ";";
  )

  | If (test, stmts) -> (
    ps env "if (";
    codegen_expression env test;
    ps env ") {\n";
    with_indent env (fun () -> 
      List.iter
        ~f:(fun stmt ->
          print_indents env;
          codegen_statement env stmt;
          endl env;
        )
        stmts
    );
    print_indents env;
    ps env "}"
  )

  | While (expr, block) -> (
    ps env "while (";
    codegen_expression env expr;
    ps env ".int_val) {\n";
    with_indent env (fun () -> 
      List.iter
        ~f:(fun stmt ->
          print_indents env;
          codegen_statement env stmt;
          endl env;
        )
        block.body
    );
    print_indents env;
    ps env "}"
  )

  | Break -> (
    print_indents env;
    ps env "break;";
    endl env;
  )

  | Continue -> (
    print_indents env;
    ps env "break;";
    endl env;
  )

  | Release expr -> (
    ps env "LCRelease(rt, ";
    codegen_expression env expr;
    ps env ");";
  )

  | _ -> ()

and codegen_declaration env decl =
  let open Decl in
  let { spec; _ } = decl in
  match spec with
  | Func _fun -> codegen_function env _fun

  | LambdaDef lambda_def -> (
    ps env (Format.sprintf "LCValue %s(LCRuntime* rt, LCValue this, int argc, LCValue* args) {\n" lambda_def.lambda_gen_name);
    ps env "    return MK_NULL();\n";
    ps env "}\n";
  )

  | Class cls -> (
    let { name; properties; original_name; _ } = cls in
    let class_id_var_name = name ^ "_class_id" in
    ps env (Format.sprintf "static LCClassID %s;\n" class_id_var_name);
    ps env (Format.sprintf "typedef struct %s {" name);
    endl env;

    with_indent env (fun () -> 
      print_indents env;
      ps env "LC_OBJ_HEADER";
      endl env;

      List.iter
        ~f:(fun prop_name ->
          print_indents env;
          ps env (Format.sprintf "LCValue %s;" prop_name);
          endl env;
        )
        properties;
    );

    ps env (Format.sprintf "} %s;" name);
    endl env;

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
      ps env (Format.sprintf "\"%s\",\n" original_name);
      print_indents env;
      ps env (Format.sprintf "%s_finalizer,\n" name);
    );
    ps env "};\n";

    ps env (Format.sprintf "LCValue %s_init(LCRuntime* rt, LCValue this, int argv, LCValue* args) {\n" name);
    with_indent env (fun () ->
      print_indents env;
      ps env (Format.sprintf "%s* obj = lc_mallocz(rt, sizeof(%s));\n" name name);
      print_indents env;
      ps env (Format.sprintf "lc_init_object(rt, %s_class_id, (LCObject*)obj);\n" name);
      print_indents env;
      ps env "return MK_CLASS_OBJ(obj);\n";
    );
    ps env "}\n";
  )

  | EnumCtor ctor -> (
    ps env (Format.sprintf "LCValue %s(LCRuntime* rt, LCValue this, int argv, LCValue* args) {\n" ctor.enum_ctor_name);
    if ctor.enum_cotr_params_size = 0 then
      ps env (Format.sprintf "    return (LCValue){ { .int_val = 0 }, (%d << 8) + 0x80 + LC_TY_NULL };\n" ctor.enum_ctor_tag_id)
    else if ctor.enum_cotr_params_size = 1 then (
      ps env "    LCValue ret = args[0];\n";
      ps env (Format.sprintf "    ret.tag += (%d << 8) + 0x80;\n" ctor.enum_ctor_tag_id);
      ps env "    return ret;\n"
    ) else (
      failwith "not implemented"
    );
    ps env "}\n";
  )

  | GlobalClassInit(init_name, init_entries) -> (
      ps env (Format.sprintf "void %s(LCRuntime* rt) {\n" init_name);

      List.iter
        ~f:(fun entry ->
          (* match entry with
          | InitClass (id_name, gen_name) ->
            ps env (Format.sprintf "    %s = LCDefineClass(rt, &%s);\n" id_name gen_name)
          | InitMethods (id_name, cls_def_name) ->
            ps env (Format.sprintf "    LCDefineClassMethod(rt, %s, %s, countof(%s));\n" id_name cls_def_name cls_def_name) *)
          ps env (Format.sprintf "    %s = LCDefineClass(rt, &%s);\n" entry.class_id_name entry.class_def_name)
        )
        init_entries;

      ps env "}\n";
  )

and codegen_symbol env sym =
  let open C_op in
  match sym with
  | SymLocal name -> ps env name
  | SymParam param_index ->
    ps env "args[";
    ps env (Int.to_string param_index);
    ps env "]"

  | SymLambda index ->
    ps env "LCLambdaGetValue(rt, this, ";
    ps env (Int.to_string index);
    ps env ")"

and codegen_expression (env: t) (expr: Expr.t) =
  let open Expr in
  let { spec; _ } = expr in
  match spec with
  | Null ->
    ps env "MK_NULL()"

  | NewInt value -> (
    ps env Primitives.Value.mk_i32;
    ps env "(";
    ps env value;
    ps env ")"
  )

  | NewChar ch -> (
    ps env "'";
    ps env (Char.to_string ch);
    ps env "'";
  )

  | NewString value -> (
    let len = String.length value in
    let value = Format.sprintf "%s(rt, (const unsigned char*)\"%s\", %d);" Primitives.Value.new_string_len value len in
    ps env value
  )

  | NewFloat value -> (
    ps env Primitives.Value.mk_f32;
    ps env "(";
    ps env value;
    ps env ")"
  )

  | NewBoolean true ->
    ps env Primitives.Constant._true

  | NewBoolean false ->
    ps env Primitives.Constant._false

  | NewLambda (c_name, params) ->
    ps env "LCNewLambda(rt, ";
    ps env c_name;
    ps env ", ";
    if Array.is_empty params then
      ps env "0, NULL"
    else (
      let params_len = Array.length params in
      ps env (Int.to_string params_len);
      ps env ", ";
      ps env "(LCValue[]) {";
      Array.iteri
        ~f:(fun index param_name->
          codegen_symbol env param_name;
          if index <> (params_len - 1) then (
            ps env ", "
          )
        )
        params;
      ps env "}";
    );
    ps env ")";

  | NewRef expr ->
    ps env "LCNewRefCell(rt, ";
    codegen_expression env expr;
    ps env ")"

  | GetRef ref ->
    ps env "LCRefCellGetValue(";
    codegen_symbol env ref;
    ps env ")"

  | NewArray len ->
    ps env "LCNewArrayLen(rt, ";
    ps env (Int.to_string len);
    ps env ")"

  | ArraySetValue (sym, index, value) -> (
    ps env "LCArraySetValue(rt, ";
    codegen_symbol env sym;
    ps env ", ";
    ps env (Int.to_string index);
    ps env ", ";
    codegen_expression env value;
    ps env ")";
  )

  | Ident value -> codegen_symbol env value

  | ExternalCall (fun_name, params) -> (
    codegen_symbol env fun_name;
    let params_len = List.length params in
    if List.is_empty params then (
      ps env (Format.sprintf "(rt, MK_NULL(), %d, NULL)" params_len);
    ) else  (
      ps env (Format.sprintf "(rt, MK_NULL(), %d, (LCValue[]) {" params_len);

      let len_m1 = params_len - 1 in

      List.iteri
        ~f:(fun index param ->
          codegen_expression env param;
          if index <> len_m1 then (
            ps env ", "
          )
        )
        params;

      ps env "})";
    )
  )

  | Call _ -> failwith "call"

  | CallLambda (callee, _params) -> (
    ps env "LCEvalLambda(rt, ";
    codegen_expression env callee;
    ps env ", 0, NULL)"
  )

  | Temp id ->
    ps env "t[";
    ps env (Int.to_string id);
    ps env "]"

  (* | Call call -> (
    let { callee; call_params; _ } = call in
    match callee.spec with
    | Expression.Identifier (sym_name, sym_id) -> (
      let ext_name_opt = Type_context.find_external_symbol env.env.ctx sym_id in
      let fun_name = match ext_name_opt with
      | Some ext_method_name -> ext_method_name
      (* it's a local function *)
      | _ -> (
        let ctor_of = Check_helper.find_construct_of env.env.ctx callee.ty_var in
        let ctor_of, _ = Option.value_exn ctor_of in
        let open Core_type.TypeDef in
        match ctor_of.spec with
        | Function _ -> failwith "function not implemented"
        | EnumCtor enum_ctor -> (
          let ctor_name = enum_ctor.enum_ctor_name ^ "_ctor" in
          ctor_name
        )
        | _ ->
          failwith (Format.sprintf "type %s: %s %d is not callable\n" sym_name ctor_of.name sym_id)
      )
      in
      pss env fun_name;
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

  ) *)

  | I32Binary(op, left, right) -> (
    let name = Primitives.Bin.prim op in
    ps env name;
    ps env "(";
    codegen_expression env left;
    ps env ", ";
    codegen_expression env right;
    ps env ")"
  )

  | Assign(name, right) -> (
    (* TODO: release the left, retain the right *)
    (let open C_op in
    match name with
    | SymLocal name ->
      ps env name;
      ps env " = ";
      codegen_expression env right

    | SymParam param_index ->
      ps env "args[";
      ps env (Int.to_string param_index);
      ps env "]";
      ps env " = ";
      codegen_expression env right

    | SymLambda index ->
      ps env "LCLambdaSetRefValue(rt, this, ";
      ps env (Int.to_string index);
      ps env ", ";
      codegen_expression env right;
      ps env ")"
    );
  )

  | Update (op, symbol, expr) -> (
    ps env "LCUpdateValue(";
    ps env (Primitives.Assign.to_arithmetic_op op);
    ps env ", ";
    (match symbol with
    | SymLocal name ->
      ps env "&";
      ps env name

    | SymParam param_index ->
      ps env "&args[";
      ps env (Int.to_string param_index);
      ps env "]"

    | SymLambda index ->
      ps env "LCLambdaGetValuePointer(rt, this, ";
      ps env (Int.to_string index);
      ps env ")"

    );
    ps env ", ";
    codegen_expression env expr;
    ps env ")"
  )

  | TagEqual (expr, tag) -> (
    ps env "LC_VALUE_TAG(";
    codegen_expression env expr;
    ps env ") == ";
    ps env (Int.to_string tag)
  )

  | IntValue e ->
    ps env "(";
    codegen_expression env e;
    ps env ").int_val"


(* return the number of temp values *)
and codegen_function_block (env: t) block =
  let open C_op.Block in
  let { body; _ } = block in
  List.iter
    ~f:(fun stmt ->
      print_indents env;
      codegen_statement env stmt;
      endl env;
    )
    body;

(* and codegen_identifier env id =
  ps env (env.scope#codegen_id id) *)

and codegen_function env (_fun: Func.t) =
  let open Func in
  ps env "LCValue ";
  ps env _fun.name;
  ps env "(LCRuntime* rt, LCValue this, int arg_len, LCValue* args)";
  ps env " {";
  endl env;

  with_indent env (fun () ->
    print_indents env;
    ps env "LCValue ret = MK_NULL();\n";

    if _fun.tmp_vars_count > 0 then (
      print_indents env;
      ps env (Format.sprintf "LCValue t[%d];\n" _fun.tmp_vars_count)
    );

    codegen_function_block env _fun.body;
    print_indents env;
    ps env "return ret;\n";
  );

  (* with_indent env (fun () ->

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

    let _stmts = codegen_function_block env _fun.body in


    (* release all local vars *)
    if vars_len_m1 >= 0 then (
      List.iter
        ~f:(fun (name, _item) ->
          print_indents env;
          (* TODO: only release GC object *)
          ps env "LCRelease(rt, ";
          ps env name;
          ps env ");";
          endl env;
        )
        vars;
      ps env ";";
      endl env;
    );

    print_indents env;
    ps env "return ret;";
    endl env;
  ); *)

  ps env "}\n"

let contents env = Buffer.contents env.buffer

let codegen_program ?indent ~ctx (declarations: Typedtree.Declaration.t list) =
  let env = create ?indent ~ctx () in
  ps env {|/* This file is auto generated by the LichenScript Compiler */
#include <stdint.h>
#include "runtime.h"
|};

  let c_decls = Transform.transform_declarations ctx declarations in

  List.iter ~f:(codegen_declaration env) c_decls.declarations;

  (* if user has a main function *)
  let main_name = Option.value_exn c_decls.main_function_name in
  ps env (main_snippet ?init_name:c_decls.global_class_init main_name);

  env
