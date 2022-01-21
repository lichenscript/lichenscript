open Lichenscript_parsing.Asttypes
open Core_type

type t = {
  loc: Loc.t;
  spec: spec;
  ctx: Type_context.t;
}

and spec =
  | NotAssignable of TypeExpr.t * TypeExpr.t
  | CannotAssignToConstVar
  | CannotReturn of TypeExpr.t * TypeExpr.t
  | CannotFindName of string
  | Redefinition of string
  | NotCallable of TypeExpr.t
  | ParamsMismatch of TypeExpr.t
  | CannotPassParam of string * TypeExpr.t * TypeExpr.t
  | CannotReadMember of string * TypeExpr.t
  | CannotApplyBinary of BinaryOp.t * TypeExpr.t * TypeExpr.t
  | CannotResolveTypeOfExpression
  | DeclareFunctionShouldSpecificExternal
  | NotAllTheCasesReturnSameType
  | CapitalizedEnumMemeber of string
  | NotAEnumConstructor of string

let make_error ctx loc spec =
  { loc; spec; ctx }

exception Error of t

module PP = struct

  (* let rec type_value ~ctx formatter ty_value =
    let open TypeExpr in
    match ty_value with
    | Unknown -> Format.pp_print_string formatter "unknown"
    | Any -> Format.pp_print_string formatter "any"
    | Ctor (var, []) -> (
      let node = Type_context.get_node ctx var in
      type_value ~ctx formatter node.value
    )
    | Ctor _ -> Format.pp_print_string formatter "ctor"
    | Ref id -> (
      let node = Type_context.get_node ctx id in
      Format.pp_print_string formatter "(typeof ";
      type_value ~ctx formatter node.value;
      Format.pp_print_string formatter ")";
    )
    | Function _ -> Format.pp_print_string formatter "function"
    | Array _ -> Format.pp_print_string formatter "array"
    | TypeDef type_sym ->
      TypeDef.pp formatter type_sym *)

  let error_spec formatter ~ctx spec =
    let pp_ty = Type_context.print_type_value ctx in
    match spec with
    | NotAssignable (be_assigned, assign) ->
      Format.fprintf formatter "Type '%s' is not assignable to type '%s'" (pp_ty assign) (pp_ty be_assigned)

    | CannotAssignToConstVar ->
      Format.fprintf formatter "Can not assign value to a const variable"

    | CannotReturn (expected, actual) ->
      Format.fprintf formatter "Type '%s' can not be returned because '%s' is expected" (pp_ty actual) (pp_ty expected)

    | CannotFindName name ->
      Format.fprintf formatter "Can not find name '%s'" name

    | Redefinition name ->
      Format.fprintf formatter "Redefinition of '%s'" name

    | NotCallable ty ->
      Format.fprintf formatter "Type '%s' is not callable" (pp_ty ty)

    | ParamsMismatch ty ->
      Format.fprintf formatter "Params provided are mismatched with definition '%s'" (pp_ty ty)

    | CannotPassParam(name, def_param, actual_param) ->
      Format.fprintf formatter "Can not pass '%s' as param '%s', because '%s' is expected"
        (pp_ty actual_param) name (pp_ty def_param)

    | CannotReadMember(name, ty) ->
      Format.fprintf formatter "Can not read member '%s' of type '%s'"
        name (pp_ty ty)

    | CannotApplyBinary (op, left ,right) ->
      Format.fprintf formatter "Can not apply %a to '%a' and '%a'"
        BinaryOp.pp op TypeExpr.pp left TypeExpr.pp right

    | CannotResolveTypeOfExpression ->
      Format.fprintf formatter "Can not resolve type of expression"

    | DeclareFunctionShouldSpecificExternal ->
      Format.fprintf formatter "Declare function should specify external symbol, use @external decorator"

    | NotAllTheCasesReturnSameType ->
      Format.fprintf formatter "All the cases of match expression should return the same type"

    | CapitalizedEnumMemeber name -> (
      let first_char = String.get name 0 in
      let upper_char = Char.uppercase_ascii first_char in
      let new_name = String.mapi (fun index ch -> if index = 0 then upper_char else ch) name in
      Format.fprintf formatter "The name of the enum member '%s' must be capitalized, try '%s'" name new_name
    )

    | NotAEnumConstructor name -> (
      Format.fprintf formatter "'%s' is not an enum constructor" name
    )

  let error ~ctx formatter err =
    let { spec; loc; _ } = err in
    let open Loc in
    Format.fprintf formatter "Error: %d:%d %a" loc.start.line loc.start.column (error_spec ~ctx) spec 
  
end
