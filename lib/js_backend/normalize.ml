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

let normalize_stmt stmt : Statement.t = stmt

let normalize_function _fun: Function.t =
  let open Function in
  let stmts = List.map ~f:normalize_stmt _fun.body.body in
  { _fun with
    body = { _fun.body with
      body = stmts;
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
          body = stmts;
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
