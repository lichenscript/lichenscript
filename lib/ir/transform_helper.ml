open Lichenscript_typing
open Lichenscript_typing.Check_helper
open Lichenscript_typing.Core_type
open Core_kernel

let type_should_not_release ~(ptr_size: Ir.ptr_size option) ctx expr =
  (* i64/f64 should not release on 64bit platform *)
  let group =
    match ptr_size with
    | Some Ir.Ptr32 ->
      [| "i32"; "u32"; "f32"; "char"; "boolean" |]
    | _ ->
      [| "i32"; "u32"; "f32"; "i64"; "f64"; "char"; "boolean" |]
  in
  let expr = Program.deref_type ctx expr in
  match expr with
  | Unit -> true
  | _ -> (
    let expr_def_opt = find_construct_of ctx expr in
    (match expr_def_opt with
      | Some(def, []) -> (
        let open TypeDef in
        def.builtin &&
        (Array.mem group ~equal:String.equal def.name)
      )
      | _ -> false
    )
  )

let type_is_not_gc ctx expr =
  match expr with
  | TypeExpr.Unit
  | TypeExpr.String -> true
  | _ ->
    begin
      let group = [| "i32"; "u32"; "f32"; "char"; "boolean"; "i64"; "f64" |] in
      let expr = Program.deref_type ctx expr in
      let expr_def_opt = find_construct_of ctx expr in
      (match expr_def_opt with
        | Some(def, []) -> (
          let open TypeDef in
          def.builtin &&
          (Array.mem group ~equal:String.equal def.name)
        )
        | _ -> false
      )
    end

