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

  | If if_spec -> codegen_expression_if env if_spec

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

  | Retain expr ->
    ps env "LCRetain(";
    codegen_expression env expr;
    ps env ");"

  | Label label ->
    ps env label;
    ps env ":"

  | Goto label ->
    ps env "goto ";
    ps env label;
    ps env ";"

  | Return expr_opt -> (
    ps env "return";
    match expr_opt with
    | Some expr ->
      ps env " ";
      codegen_expression env expr;
      ps env ";"

    | None -> ps env ";"

  )


and codegen_expression_if env if_spec =
  let open C_op.Stmt in
  let { if_test; if_consequent; if_alternate } = if_spec in
  ps env "if (";
  codegen_expression env if_test;
  ps env ") {\n";
  with_indent env (fun () -> 
    List.iter
      ~f:(fun stmt ->
        print_indents env;
        codegen_statement env stmt;
        endl env;
      )
      if_consequent
  );
  print_indents env;
  ps env "}";
  match if_alternate with
  | Some (If_alt_block blk) -> (
    ps env " else {";
    endl env;
    with_indent env (fun () -> 
      List.iter
        ~f:(fun stmt ->
          print_indents env;
          codegen_statement env stmt;
          endl env;
        )
        blk
    );
    print_indents env;
    ps env "}";
  )
  | Some (If_alt_if if_spec) -> (
    ps env " else ";
    codegen_expression_if env if_spec
  )
  | None -> ()

and codegen_declaration env decl =
  let open Decl in
  let { spec; _ } = decl in
  match spec with
  | Func _fun -> codegen_function env _fun

  | FuncDecl fun_name ->
    ps env "LCValue ";
    codegen_symbol env fun_name;
    ps env "(LCRuntime* rt, LCValue this, int argc, LCValue* args);\n"

  | LambdaDef lambda_def -> (
    ps env (Format.sprintf "LCValue %s(LCRuntime* rt, LCValue this, int argc, LCValue* args) {\n" lambda_def.lambda_gen_name);
    ps env "    return MK_NULL();\n";
    ps env "}\n";
  )

  | Class cls -> (
    let { name; properties; original_name; finalizer; _ } = cls in
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

    (match finalizer with
    | Some finalizer -> (
      ps env (Format.sprintf "void %s(LCRuntime* rt, LCValue this) {\n" finalizer.finalizer_name);
      with_indent env (fun () ->
        List.iter
        ~f:(fun stmt ->
          print_indents env;
          codegen_statement env stmt;
          endl env
        )
        finalizer.finalizer_content
      );
      ps env "}\n";
    )
    | None -> ()
    );

    let class_def_name = name ^ "_def" in
    ps env (Format.sprintf "static LCClassDef %s = {\n" class_def_name);
    with_indent env (fun () ->
      print_indents env;
      ps env (Format.sprintf "\"%s\",\n" original_name);
      print_indents env;
      (match finalizer with
      | Some finalizer ->
        ps env finalizer.finalizer_name
      | None ->
        ps env "MK_NULL()"
      );
      ps env ",\n";
    );
    ps env "};\n";

    ps env (Format.sprintf "LCValue %s_init(LCRuntime* rt) {\n" name);
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
      ps env (Format.sprintf "    return MK_UNION(%d);\n" ctor.enum_ctor_tag_id)
    else (
      ps env (Format.sprintf "    return LCNewUnionObject(rt, %d, argv, args);\n" ctor.enum_ctor_tag_id)
    );
    ps env "}\n";
  )

  | GlobalClassInit(init_name, init_entries) -> (
    List.iter
      ~f:(fun entry ->
        let { class_name; class_methods; _} = entry in
        if not (List.is_empty class_methods) then (
          ps env (Format.sprintf "static LCClassMethodDef %s_methods[] = {\n" class_name);
          with_indent env (fun () ->
            List.iter
              ~f:(fun m ->
                print_indents env;
                ps env "{ \"";
                ps env m.class_method_name;
                ps env "\", 0, ";
                ps env m.class_method_gen_name;
                ps env " },\n"
              )
              class_methods
          );
          ps env "};\n"
        )
      )
      init_entries;

    ps env (Format.sprintf "void %s(LCRuntime* rt) {\n" init_name);

    List.iter
      ~f:(fun entry ->
        ps env (Format.sprintf "    %s = LCDefineClass(rt, &%s);\n" entry.class_id_name entry.class_def_name);
        if not (List.is_empty entry.class_methods) then (
          ps env (Format.sprintf "    LCDefineClassMethod(rt, %s, %s_methods, countof(%s_methods));\n" entry.class_id_name entry.class_name entry.class_name)
        )
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

  | SymThis -> ps env "this"

and codegen_expression (env: t) (expr: Expr.t) =
  let open Expr in
  match expr with
  | Null ->
    ps env "MK_NULL()"

  | NewInt value -> (
    ps env Primitives.Value.mk_i32;
    ps env "(";
    ps env value;
    ps env ")"
  )

  | NewChar ch -> (
    ps env "MK_CHAR('";
    ps env (Char.to_string ch);
    ps env "')";
  )

  | NewString value -> (
    let len = String.length value in
    let value = Format.sprintf "%s(rt, (const unsigned char*)\"%s\", %d)" Primitives.Value.new_string_len value len in
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

  | NewLambda (c_name, this, params) ->
    ps env "LCNewLambda(rt, ";
    ps env c_name;
    ps env ", ";
    codegen_expression env this;
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

  | NewMap init_size ->
    ps env "lc_std_map_new(rt, LC_TY_STRING, ";
    ps env (Int.to_string init_size);
    ps env ")"

  | Not expr ->
    ps env "LC_NOT(";
    codegen_expression env expr;
    ps env ")"

  | ArrayGetValue (sym, index) -> (
    ps env "LCArrayGetValue(rt, ";
    codegen_expression env sym;
    ps env ", ";
    codegen_expression env index;
    ps env ")"
  )

  | Ident value -> codegen_symbol env value

  | ExternalCall (fun_name, ths, params) -> (
    codegen_symbol env fun_name;
    let params_len = List.length params in
    ps env "(rt, ";
    (match ths with
    | Some e -> codegen_expression env e
    | None ->
      ps env "MK_NULL()"
    );
    ps env ", ";
    if List.is_empty params then (
      ps env (Int.to_string params_len);
      ps env ", NULL)"
    ) else  (
      ps env (Format.sprintf "%d, (LCValue[]) {" params_len);

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

  | InitCall sym -> (
    codegen_symbol env sym;
    ps env "(rt)"
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

  | I32Binary(op, left, right) -> (
    let name = Primitives.Bin.prim op in
    ps env name;
    ps env "(";
    codegen_expression env left;
    ps env ", ";
    codegen_expression env right;
    ps env ")"
  )

  | Assign(Expr.Ident name, right) -> (
    (* TODO: release the left, retain the right *)
    (match name with
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

    | SymThis -> failwith "impossible assgning to this"
    );
  )

  | Assign(left, right) ->
    codegen_expression env left;
    ps env " = ";
    codegen_expression env right

  | Update (op, C_op.Expr.Ident symbol, expr) -> (
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

    | SymThis -> failwith "impossible to updating this"

    );
    ps env ", ";
    codegen_expression env expr;
    ps env ")"
  )

  | Update _ -> failwith "unrechable"

  | TagEqual (expr, tag) -> (
    ps env "LCUnionGetType(";
    codegen_expression env expr;
    ps env ") == ";
    ps env (Int.to_string tag)
  )

  | UnionGet (expr, index) -> (
    ps env "LCUnionObjectGet(rt, ";
    codegen_expression env expr;
    ps env ", ";
    ps env (Int.to_string index);
    ps env ")"
  )

  | IntValue e ->
    ps env "(";
    codegen_expression env e;
    ps env ").int_val"

  | GetField(expr, cls_name, field_name) -> (
    ps env "LCCast(";
    codegen_expression env expr;
    ps env ", ";
    ps env cls_name;
    ps env "*";
    ps env ")->";
    ps env field_name
  )

  | StringCmp(op, left, right) -> (
    ps env "lc_std_string_cmp(rt, ";
    ps env (Primitives.Bin.to_cmp op);
    ps env ", ";
    codegen_expression env left;
    ps env ", ";
    codegen_expression env right;
    ps env ")"
  )

  | StringEqUtf8(expr, expected) -> (
    ps env "LCStringEqUtf8(rt, ";
    codegen_expression env expr;
    ps env ", \"";
    ps env expected;
    ps env "\", ";
    ps env (Int.to_string (String.length expected));
    ps env ")"
  )

  | Retaining expr -> (
    ps env "(LCRetain(";
    codegen_expression env expr;
    ps env "), ";
    codegen_expression env expr;
    ps env ")"
  )

  | Invoke (expr, name, params) -> (
    ps env "LCInvokeStr(rt, ";
    codegen_expression env expr;
    ps env ", \"";
    ps env name;
    ps env "\", ";
    let params_len = List.length params in
    if params_len = 0 then
      ps env "0, NULL)"
    else (
      ps env (Int.to_string params_len);
      ps env ", ";
      ps env "(LCValue[]) {";
      List.iteri
        ~f:(fun index param_name->
          codegen_expression env param_name;
          if index <> (params_len - 1) then (
            ps env ", "
          )
        )
        params;
      ps env "})";
    )
  )

(* return the number of temp values *)
and codegen_function_block (env: t) block =
  let open C_op.Block in
  let { body; _ } = block in
  List.iter
    ~f:(fun stmt ->
      (match stmt.spec with
      | Label _ -> ()
      | _ -> print_indents env);
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
    ps env "LCValue ret;\n";

    if _fun.tmp_vars_count > 0 then (
      print_indents env;
      ps env (Format.sprintf "LCValue t[%d] = {0};\n" _fun.tmp_vars_count)
    );

    codegen_function_block env _fun.body;
  );

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
  let main_name =
    Option.value_exn
      ~message:"can not find main function"
      c_decls.main_function_name
  in
  ps env (main_snippet ?init_name:c_decls.global_class_init main_name);

  env
