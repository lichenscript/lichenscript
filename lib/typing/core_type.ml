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
open Lichenscript_lex

module PropsMap = Hashtbl.Make(String)

module Visibility = struct

  type t =
    | Public
    | Protected
    | Private
    | Internal
  [@@deriving show]

  let access_in_module = function
  | Public
  | Internal -> true
  | _ -> false

end

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
    | Lambda of params * t

    (*
     * Instance of a class method
     *
     * class Array <T> {
     *   push(item: T)
     * }
     *
     * For Array<i32>, a method instance is ::push(item: i32)
     *
     *)
    | Method of TypeDef.t * params * t

    | Tuple of t list

    (*
     * Alias of Array<T>
     *)
    | Array of t

    | String

    | TypeDef of TypeDef.t

    (* used for generic *)
    | TypeSymbol of string

  and params = {
    params_content: (string * TypeExpr.t) list;
    params_rest: (string * TypeExpr.t) option;
  }

  val pp: Format.formatter -> t -> unit

end = struct

  type t =
    | Unknown
    | Any
    | Ctor of t * (t list)
    | Ref of int
    | Lambda of params * t
    | Method of TypeDef.t * params * t
    | Tuple of t list
    | Array of t
    | String
    | TypeDef of TypeDef.t
    | TypeSymbol of string

  and params = {
    params_content: (string * TypeExpr.t) list;
    params_rest: (string * TypeExpr.t) option;
  }

  (* only used internally *)
  let rec pp formatter t =
    match t with
    | Unknown -> Format.fprintf formatter "unknown"
    | Any -> Format.fprintf formatter "any"

    | Ctor(t, vars) ->
      Format.fprintf formatter "%a<%d>" pp t (List.length vars)

    | Ref i -> Format.fprintf formatter "ref '%d" i
    | Lambda _ -> Format.fprintf formatter "lambda"
    | Method (_method, _, _) -> Format.fprintf formatter "::%s" TypeDef.(_method.name)
    | Tuple childrens -> (
      let children_len = List.length childrens in
      Format.fprintf formatter "(";
      List.iteri
        ~f:(fun index item ->
          pp formatter item;
          if index <> (children_len - 1) then (
            Format.fprintf formatter ", "
          )
        )
        childrens;
      Format.fprintf formatter ")"
    )

    | Array t -> Format.fprintf formatter "%a[]" pp t
    | String -> Format.fprintf formatter "string"
    | TypeDef def -> Format.fprintf formatter "typedef(%a)" TypeDef.pp def
    | TypeSymbol sym -> Format.pp_print_string formatter sym

end

and TypeDef : sig
  type module_type = {
    props: TypeExpr.t PropsMap.t;
  }

  and enum_type = {
    enum_members: (string * t) list;
    enum_params: string list;
    enum_methods: (string * Visibility.t * t) list;
  }

  and _function = {
    fun_vars:   string list;
    fun_params: TypeExpr.params;
    fun_return: TypeExpr.t;
  }

  and class_elm =
    | Cls_elm_prop of Visibility.t * int * TypeExpr.t
    | Cls_elm_method of Visibility.t * t
    | Cls_elm_get_set of Visibility.t * t option * t option

  and class_type = {
    tcls_name:            string;
    tcls_vars:            string list;
    tcls_extends:         TypeExpr.t option;
    tcls_implements:      (TypeExpr.t * Loc.t) list;
    tcls_elements:        (string * class_elm) list;
    tcls_static_elements: (string * class_elm) list;
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
    method_params: TypeExpr.params;
    method_return: TypeExpr.t;
  }

  and interface = {
    intf_methods: (string * class_elm) list;
  }

  and spec =
    | Primitive
    | Class of class_type 
    | ClassMethod of class_method
    | Interface of interface
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

  and enum_type = {
    enum_members: (string * t) list;
    enum_params: string list;
    enum_methods: (string * Visibility.t * t) list;
  }

  and _function = {
    fun_vars:   string list;
    fun_params: TypeExpr.params;
    fun_return: TypeExpr.t;
  }

  and class_elm =
    | Cls_elm_prop of Visibility.t * int * TypeExpr.t
    | Cls_elm_method of Visibility.t * t
    | Cls_elm_get_set of Visibility.t * t option * t option

  and class_type = {
    tcls_name:            string;
    tcls_vars:            string list;
    tcls_extends:         TypeExpr.t option;
    tcls_implements:      (TypeExpr.t * Loc.t) list;
    tcls_elements:        (string * class_elm) list;
    tcls_static_elements: (string * class_elm) list;
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
    method_params: TypeExpr.params;
    method_return: TypeExpr.t;
  }

  and interface = {
    intf_methods: (string * class_elm) list;
  }

  and spec =
    | Primitive
    | Class of class_type 
    | ClassMethod of class_method
    | Interface of interface
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
