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
open Lichenscript_parsing.Asttypes

type spec =
  | Dg_error of Type_error.t
  | Dg_warning of Type_warning.t

type t = {
  loc: Loc.t;
  spec: spec;
  ctx: Type_context.t;
}

let make_error ctx loc err_spec =
  { loc; spec = Dg_error err_spec; ctx }

exception Error of t

module PP = struct

  let error_spec formatter ~ctx spec =
    let open Type_error in
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

    | CannotGetIndex ty ->
      let left_type = Type_context.print_type_value ctx ty in
      Format.fprintf formatter "Can not get index of type '%s'" left_type

    | CannotImplement ty ->
      let left_type = Type_context.print_type_value ctx ty in
      Format.fprintf formatter "'%s' is not an interface, can not implement" left_type

    | CannotUsedAsKeyOfMap ty ->
      let left_type = Type_context.print_type_value ctx ty in
      Format.fprintf formatter "Type '%s' can not be used ad key of Map." left_type

    | CannotAccessBeforeInit var_name ->
      Format.fprintf formatter "Cannot access '%s' before initialization." var_name

    | CannotFindNameForImport(local_name, find_name) ->
      Format.fprintf formatter "Cannot find '%s' for module '%s'." find_name local_name

    | CannotCastType(expr_type, cast_type) ->
      let expr_type = Type_context.print_type_value ctx expr_type in
      let cast_type = Type_context.print_type_value ctx cast_type in
      Format.fprintf formatter "Cannot cast type '%s' to '%s'." expr_type cast_type

    | MissingMethodForInterface(intf_name, method_name) ->
      Format.fprintf formatter "Missing method '%s' for interface '%s'." method_name intf_name

    | CannotContructParamOfType(name, ty) ->
      Format.fprintf formatter "Can not construct param %s type '%s'" name (pp_ty ty)

    | PrivateVirtualMethod method_name ->
      Format.fprintf formatter "The method '%s' is private, it can't be 'virtual' or 'override'." method_name

    | InvalidAssign ->
      Format.fprintf formatter "The left-hand side of an assignment expression must be a variable or a property access."

    | OnlyAssignArrayIndexAlpha ->
      Format.fprintf formatter "Currently you can only assign index to an array."

    | OnlyI32InIndexAlpha ->
      Format.fprintf formatter "Currently you can only use i32 as index."

    | IsNotGeneric name ->
      Format.fprintf formatter "'%s' is not generic." name

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
      let left_type = Type_context.print_type_value ctx left in
      let right_type = Type_context.print_type_value ctx right in
      Format.fprintf formatter "Can not apply '%s' to type '%s' and '%s'"
        (BinaryOp.to_string op) left_type right_type

    | CannotApplyUnary (op, ty) ->
      Format.fprintf formatter "Can not apply '%a' to type '%s'"
        UnaryOp.pp op (pp_ty ty)

    | CannotResolveTypeOfExpression ->
      Format.fprintf formatter "Can not resolve type of expression"

    | DeclareFunctionShouldSpecificExternal ->
      Format.fprintf formatter "Declare function should specify external symbol, use @external decorator"

    | NotAllTheCasesReturnSameType (prev, current) ->
      Format.fprintf formatter "All the cases of match expression should return the same type, previous is %s, but got %s"
        (pp_ty prev) (pp_ty current)

    | CapitalizedEnumMember name -> (
      let first_char = String.get name 0 in
      let upper_char = Char.uppercase_ascii first_char in
      let new_name = String.mapi (fun index ch -> if index = 0 then upper_char else ch) name in
      Format.fprintf formatter "The name of the enum member '%s' must be capitalized, try '%s'" name new_name
    )

    | LowercaseTheImportName import_name ->
      Format.fprintf formatter "Lowercase the first char of the import name '%s'" import_name

    | LowercaseTheFunctionName fun_name ->
      Format.fprintf formatter "Lowercase the first char of the function name '%s'" fun_name

    | LowercaseTheMethod (cls_or_enum, cls_name, method_name) ->
      Format.fprintf formatter "Lowercase the first char of the method name '%s' of the %s '%s'" method_name cls_or_enum cls_name

    | NotAEnumConstructor name -> (
      Format.fprintf formatter "'%s' is not an enum constructor" name
    )

    | RestParamsMustAtLast ->
      Format.fprintf formatter "A rest parameter must be last in a parameter list."

    | ParamDoesNotProvided param_name ->
      Format.fprintf formatter "The parameter %s is missing." param_name

    | UnexpectedParams(expected, actual) ->
      Format.fprintf formatter "Expected %d arguments, but got %d." expected actual

    | RestShouldBeArray ->
      Format.fprintf formatter "A rest parameter must be of an array type."

    | ClassPropNotInit(cls_name, prop_name) ->
      Format.fprintf formatter "The property '%s' of class %s is not initialized." prop_name cls_name

    | ClassPropRedefinition(cls_name, prop_name) ->
      Format.fprintf formatter "Redefine property '%s' of class '%s'." prop_name cls_name

    | ClassInitNotAssignable (cls_name, prop_name, be_assigned, assign) ->
      Format.fprintf formatter "Init propperty '%s' of class '%s' failed, type '%s' is not assignable to type '%s'"
        prop_name cls_name
        (pp_ty assign) (pp_ty be_assigned)

    | CannotBindingOfPattern pattern_name ->
      Format.fprintf formatter "Currently can not bind '%s', will support in the future." pattern_name

    | UnexpectedPatternType(pat_name, ty) ->
      Format.fprintf formatter "Unexpected pattern type '%s', expected '%s'." pat_name (pp_ty ty)

    | PatternNotExausted ->
      Format.fprintf formatter "Pattern is not exausted."

    | WhileTestShouldBeBoolean ty ->
      Format.fprintf formatter "The type of expression after while should be 'boolean', but got '%s'." (pp_ty ty)

    | IfTestShouldBeBoolean ty ->
      Format.fprintf formatter "The type of expression after if should be 'boolean', but got '%s'." (pp_ty ty)

    | CannotResolverReference name ->
      Format.fprintf formatter "Can not resolve reference '%s'." name
    
    | CannotUsedForTryExpression ty ->
      Format.fprintf formatter "The type '%s' can not be used in a try expression." (pp_ty ty)

  let error ~ctx formatter diagnosis =
    let { spec; loc; _ } = diagnosis in
    match spec with
    | Dg_warning _ -> ()
    | Dg_error err_spec -> (
      let open Loc in
      Format.fprintf formatter "Error: %d:%d %a" loc.start.line loc.start.column (error_spec ~ctx) err_spec 
    )

end
