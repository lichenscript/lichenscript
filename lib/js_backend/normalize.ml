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
 open Core_kernel
 open Lichenscript_typing.Typedtree

 (*
  * Some control-flow operation is allowed in LichenScript
  * But it's impossbile in JavaScript, such as
  *
  * 1. Control-Flow in an expression
  *
  *   For examples:
  *   - break a while loop in an expression
  *   - return function in an expression
  *
  *   const c = if a > b { break } else { a }
  *   print(c)
  *
  *   So we need to "normalize" it into:
  *
  *   if (a > b) {
  *     break;
  *   } else {
  *     const c = a;
  *     print(c)
  *   }
  *
  *  2. Pattern matching
  *
  *    Transform pattern matching into if statements.
  *
  *)

module Continuation = struct
  [@warning "-unused-type-declaration"]
  [@warning "-unused-value-declaration"]

  type generateor = (Statement.t list -> Statement.t list)
  type t = (generateor -> Statement.t list)

  let (>>=) (left: t) (right: t): t = 
    (fun generateor ->
      left (fun left_finalizers ->
        right (fun right_finalizers ->
          let merged_finalizers = List.append left_finalizers right_finalizers in
          generateor merged_finalizers
        )
      ) 
    )
  
end

(*
 * has break/continue/return
 *)
module ControlFlow = struct
  [@warning "-unused-value-declaration"]

  let rec has_control_flow (expr: Expression.t): bool =
    let open Expression in
    match expr.spec with
    | Lambda _ -> failwith "un"

    | If if_desc -> if_desc_has_control_flow if_desc

    | Array _
    | Map _
    | Call _
    | Tuple _
    | Member _
    | Index _
    | Unary _
    | Binary _
    | Assign _
    | Block _
    | Init _
    | Match _
    | Identifier _
    | Constant _
    | This
    | Super ->
      false

  and block_has_control_flow _blk = false

  and if_desc_has_control_flow if_desc =
    let open Expression in
    let { if_test; if_consequent; if_alternative; _ } = if_desc in
    (has_control_flow if_test) &&
    (block_has_control_flow if_consequent) &&
    (match if_alternative with
    | Some (If_alt_if if_desc) -> if_desc_has_control_flow if_desc
    | Some (If_alt_block blk) -> block_has_control_flow blk
    | None -> false
    )
  
end

let normalize_expr expr : Statement.t list * Expression.t =
  let open Expression in
  match expr.spec with
  | Lambda _
  | If _
  | Array _
  | Map _
  | Call _
  | Tuple _
  | Member _
  | Index _
  | Unary _
  | Binary _
  | Assign _
  | Block _
  | Init _
  | Match _
  | Identifier _
  | Constant _
  | This
  | Super -> [], expr

let rec normalize_stmt stmt : Statement.t list =
  let open Statement in
  match stmt.spec with
  | Expr expr ->
    let stmts, expr' = normalize_expr expr in
    List.append
      stmts
      [{ stmt with
        spec = Expr expr';
      }]

  | Semi expr ->
    let stmts, expr' = normalize_expr expr in
    List.append
      stmts
      [{ stmt with
        spec = Semi expr';
      }]

  | While while_desc -> (
    let { while_test; while_block; _ } = while_desc in
    let stmts, new_test = normalize_expr while_test in
    List.append
      stmts
      [{ stmt with
        spec = While { while_desc with
          while_test = new_test;
          while_block = { while_block with
            body =
              while_block.body
              |> List.map ~f:normalize_stmt
              |> List.concat;
          };
        };
      }]
  )

  | Binding binding -> (
    let stmts, expr' = normalize_expr binding.binding_init in
    List.append
      stmts
      [{ stmt with
        spec = Binding { binding with
          binding_init = expr';
        };
      }]
  )

  | Return expr_opt -> (
    match expr_opt with
    | Some expr ->
      let stmts, expr' = normalize_expr expr in
      List.append
        stmts
        [{ stmt with
          spec = Return (Some expr');
        }]

    | None -> [stmt]

  )

  (* default: *)
  | Debugger
  | Continue _
  | Break _
  | Empty -> [stmt]

let normalize_function _fun: Function.t =
  let open Function in
  let stmts = List.map ~f:normalize_stmt _fun.body.body in
  { _fun with
    body = { _fun.body with
      body = List.concat stmts;
    }
  }

let normalize_class (cls: Declaration._class) : Declaration._class =
  let normalize_class_element elm =
    let open Declaration in
    match elm with
    | Cls_method _method -> (
      let { cls_method_body; _ } = _method in
      let stmts = List.map ~f:normalize_stmt cls_method_body.body in

      Cls_method { _method with
        cls_method_body = { _method.cls_method_body with
          body = List.concat stmts;
        }
      }
    )

    |_ -> elm
  in

  let cls_body_elements = cls.cls_body.cls_body_elements in

  { cls with
    cls_body = { cls.cls_body with
      cls_body_elements = List.map ~f:normalize_class_element cls_body_elements;
    }
  }

let normalize_declaration decl: Declaration.t =
  let open Declaration in
  match decl.spec with
  | Function_ _function ->
    { decl with
      spec = Function_ (normalize_function _function);
    }

  | Class cls ->
    { decl with
      spec = Class (normalize_class cls);
    }

  | _ -> decl

let normalize (decls: Declaration.t list) =
  List.map ~f:normalize_declaration decls
