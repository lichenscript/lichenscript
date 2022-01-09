open Lichenscript_lex
open Core_type

type t = {
  loc: Loc.t;
  spec: spec;
  ctx: Type_context.t;
}

and spec =
  | NotAssignable of TypeExpr.t * TypeExpr.t
  | CannotReturn of TypeExpr.t * TypeExpr.t
  | CannotFindName of string
  | Redefinition of string
  | NotCallable of TypeExpr.t
  | ParamsMismatch of TypeExpr.t
  | CannotPassParam of string * TypeExpr.t * TypeExpr.t
  | CannotReadMember of string * TypeExpr.t
  | NotAddable of TypeDef.t * TypeDef.t
  | CannotResolveTypeOfExpression
  | DeclareFunctionShouldSpecificExternal

let make_error ctx loc spec =
  { loc; spec; ctx }

exception Error of t

module PP = struct

  let rec type_value ~ctx formatter ty_value =
    let open TypeExpr in
    match ty_value with
    | Unknown -> Format.pp_print_string formatter "unknown"
    | Any -> Format.pp_print_string formatter "any"
    | Ctor (var, []) -> (
      let node = Type_context.get_node ctx var in
      type_value ~ctx formatter node.value
    )
    | Ctor _ -> Format.pp_print_string formatter "ctor"
    | Class _ -> Format.pp_print_string formatter "class"
    | Function _ -> Format.pp_print_string formatter "function"
    | Module _ -> Format.pp_print_string formatter "module"
    | Array _ -> Format.pp_print_string formatter "array"
    | TypeDef type_sym ->
      TypeDef.pp formatter type_sym

  let error_spec formatter ~ctx spec =
    let pp_ty = type_value ~ctx in
    match spec with
    | NotAssignable (be_assigned, assign) ->
      Format.fprintf formatter "Type '%a' is not assignable to type '%a'" pp_ty assign pp_ty be_assigned

    | CannotReturn (expected, actual) ->
      Format.fprintf formatter "Type '%a' can not be returned because '%a' is expected" pp_ty actual pp_ty expected

    | CannotFindName name ->
      Format.fprintf formatter "Can not find name '%s'" name

    | Redefinition name ->
      Format.fprintf formatter "Redefinition of '%s'" name

    | NotCallable ty ->
      Format.fprintf formatter "Type '%a' is not callable" pp_ty ty 

    | ParamsMismatch ty ->
      Format.fprintf formatter "Params provided are mismatched with definition '%a'" pp_ty ty 

    | CannotPassParam(name, def_param, actual_param) ->
      Format.fprintf formatter "Can not pass '%a' as param '%s', because '%a' is expected"
        (type_value ~ctx) actual_param name pp_ty def_param

    | CannotReadMember(name, ty) ->
      Format.fprintf formatter "Can not read member '%s' of type '%a'"
        name pp_ty ty

    | NotAddable (left ,right) ->
      Format.fprintf formatter "Type '%a' can not add '%a'"
        TypeDef.pp left TypeDef.pp right

    | CannotResolveTypeOfExpression ->
      Format.fprintf formatter "Can not resolve type of expression"

    | DeclareFunctionShouldSpecificExternal ->
      Format.fprintf formatter "declare function should specify external symbol, use @external decorator"

  let error ~ctx formatter err =
    let { spec; loc; _ } = err in
    let open Loc in
    Format.fprintf formatter "Error: %d:%d %a" loc.start.line loc.start.column (error_spec ~ctx) spec 
  
end
