open Lichenscript_parsing.Asttypes
open Core_type

type t =
  | NotAssignable of TypeExpr.t * TypeExpr.t
  | CannotAssignToConstVar
  | CannotReturn of TypeExpr.t * TypeExpr.t
  | CannotFindName of string
  | CannotGetIndex of TypeExpr.t
  | CannotContructParamOfType of string * TypeExpr.t
  | CannotImplement of TypeExpr.t
  | CannotUsedAsKeyOfMap of TypeExpr.t
  | CannotApplyUnary of UnaryOp.t * TypeExpr.t
  | CannotAccessBeforeInit of string
  | MissingMethodForInterface of string * string  (* interface_name, method_name *)
  | InvalidAssign
  | OnlyAssignArrayIndexAlpha
  | OnlyI32InIndexAlpha
  | IsNotGeneric of string
  | Redefinition of string
  | NotCallable of TypeExpr.t
  | ParamsMismatch of TypeExpr.t
  | CannotPassParam of string * TypeExpr.t * TypeExpr.t
  | CannotReadMember of string * TypeExpr.t
  | CannotApplyBinary of BinaryOp.t * TypeExpr.t * TypeExpr.t
  | CannotResolveTypeOfExpression
  | DeclareFunctionShouldSpecificExternal
  | NotAllTheCasesReturnSameType of (TypeExpr.t * TypeExpr.t)
  | CapitalizedEnumMemeber of string
  | NotAEnumConstructor of string
  | RestParamsMustAtLast
  | ParamDoesNotProvided of string
  | UnexpectedParams of int * int
  | RestShouldBeArray
  | ClassPropNotInit of string * string
  | ClassPropRedefinition of string * string
  | ClassInitNotAssignable of string * string * TypeExpr.t * TypeExpr.t
  | CannotBindingOfPattern of string
  | UnexpectedPatternType of string * TypeExpr.t
  | PatternNotExausted
  | WhileTestShouldBeBoolean of TypeExpr.t
  | IfTestShouldBeBoolean of TypeExpr.t
