(*
 * This file is part of LichenScript Compiler.
 *
 * LichenScript Compiler is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * LichenScript Compiler is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with LichenScript Compiler. If not, see <https://www.gnu.org/licenses/>.
 *)
open Core_kernel

module PropsMap = Hashtbl.Make(String)

(*
 * Type expression is expressing something behind an expression
 * 
 * For example:
 * let string_to_int: (a: string) => i32
 * the expression after the semicolon is called `TypeExpr`
 *
 * Every expression has a TypeExpr
 *)
module rec TypeExpr : sig
  type t =
    | Unknown
    | Any

    (*
     * Ctor represents `instance`, does not represent the type itself
     *
     * For example:
     * let a : Person => Ctor(Person, [])
     *
     * saying `a` is an instance of Person
     *
     * One thing to notice, contants are also instances:
     * 1,2,3 are instances of i32
     * "string" is instances of String
     *)
    | Ctor of t * (t list)

    (*
     * 1. On the contract of Ctor, `Ref` represents the original type itself.
     *    I guess this is used internally, such as referring a method of a class.
     *
     * 2. Ref also represents reference to another TypeExpr, for example:
     *
     *    let a: '1 = "hello" '2;
     *
     *    Node '1 is referring Node '2
     *
     *)
    | Ref of int

    (*
     * (a: string) => i32
     *)
    | Lambda of t list * t

    (*
     * Alias of Array<T>
     *)
    | Array of t

    | String

    | TypeDef of TypeDef.t

    (* used for generic *)
    | TypeSymbol of string

  and function_type = {
    tfun_params: (string * t) list;
    tfun_ret: t;
  }

  val pp: Format.formatter -> t -> unit

end = struct

  type t =
    | Unknown
    | Any
    | Ctor of t * (t list)
    | Ref of int
    | Lambda of t list * t
    | Array of t
    | String
    | TypeDef of TypeDef.t
    | TypeSymbol of string

  and function_type = {
    tfun_params: (string * t) list;
    tfun_ret: t;
  }

  (* only used internally *)
  let rec pp formatter t =
    match t with
    | Unknown -> Format.fprintf formatter "unknown"
    | Any -> Format.fprintf formatter "any"
    | Ctor _ -> Format.fprintf formatter "ctor"
    | Ref i -> Format.fprintf formatter "ref '%d" i
    | Lambda _ -> Format.fprintf formatter "lambda"
    | Array t -> Format.fprintf formatter "%a[]" pp t
    | String -> Format.fprintf formatter "string"
    | TypeDef _ -> Format.fprintf formatter "typedef"
    | TypeSymbol sym -> Format.pp_print_string formatter sym

end

and TypeDef : sig
  type module_type = {
    props: TypeExpr.t PropsMap.t;
  }

  and enum_member = {
    enum_mem_name: string;
    enum_mem_fields: t list;
  }

  and enum_type = {
    enum_members: enum_member list;
    enum_params: string list;
  }

  and _function = {
    fun_params: TypeExpr.t list;
    fun_return: TypeExpr.t;
  }

  and class_type = {
    tcls_name:            string;
    tcls_extends:         TypeExpr.t option;
    tcls_elements:        (string * int) list;
    tcls_static_elements: (string * int) list;
  }

  and enum_ctor = {
    enum_ctor_name: string;
    enum_ctor_tag_id: int;
    enum_ctor_super_id: int;
    enum_ctor_params: TypeExpr.t list;
  }

  and method_get_set =
   | Getter
   | Setter

  and class_method = {
    method_cls_id: int;
    method_get_set: method_get_set option;
    method_is_virtual: bool;
    method_params: TypeExpr.t list;
    method_return: TypeExpr.t;
  }

  and spec =
    | Primitive
    | Class of class_type 
    | ClassMethod of class_method
    | Alias of TypeExpr.t
    | Module_ of module_type
    | Function of _function
    | Enum of enum_type
    | EnumCtor of enum_ctor

  and t = {
    id: int;
    builtin: bool;
    name: string;
    spec: spec;
  }

  val name: t -> string

  val builtin: t -> bool

  val create: ?builtin:bool -> int -> string -> spec -> t

  val create_module: unit -> module_type

  val pp: Format.formatter -> t -> unit

  val (==): t -> t -> bool

end = struct
  type module_type = {
    props: TypeExpr.t PropsMap.t;
  }

  and enum_member = {
    enum_mem_name: string;
    enum_mem_fields: t list;
  }

  and enum_type = {
    enum_members: enum_member list;
    enum_params: string list;
  }

  and _function = {
    fun_params: TypeExpr.t list;
    fun_return: TypeExpr.t;
  }

  and class_type = {
    tcls_name:            string;
    tcls_extends:         TypeExpr.t option;
    tcls_elements:        (string * int) list;
    tcls_static_elements: (string * int) list;
  }

  and enum_ctor = {
    enum_ctor_name: string;
    enum_ctor_tag_id: int;
    enum_ctor_super_id: int;
    enum_ctor_params: TypeExpr.t list;
  }

  and method_get_set =
   | Getter
   | Setter

  and class_method = {
    method_cls_id: int;
    method_get_set: method_get_set option;
    method_is_virtual: bool;
    method_params: TypeExpr.t list;
    method_return: TypeExpr.t;
  }

  and spec =
    | Primitive
    | Class of class_type 
    | ClassMethod of class_method
    | Alias of TypeExpr.t
    | Module_ of module_type
    | Function of _function
    | Enum of enum_type
    | EnumCtor of enum_ctor

  and t = {
    id: int;
    builtin: bool;
    name: string;
    spec: spec;
  }

  let name sym = sym.name

  let builtin sym = sym.builtin

  let create ?(builtin=false) id name spec =
    {
      id;
      builtin;
      name;
      spec;
    }

  let create_module () =
    {
      props = PropsMap.create ();
    }

  let pp formatter (sym: t) =
    Format.pp_print_string formatter sym.name

  let (==) left right =
    match (left.builtin, right.builtin) with
    | (true, true) ->
      String.equal left.name right.name

    | (false, false) ->
      phys_equal left right

    | _ -> false
  
end

type node = {
  loc: Lichenscript_lex.Loc.t;
  value: TypeExpr.t;
  deps: int list;
}

let unknown = {
  loc = Lichenscript_lex.Loc.none;
  value = TypeExpr.Unknown;
  deps = [];
}
