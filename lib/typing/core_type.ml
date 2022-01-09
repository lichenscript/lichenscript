open Core_kernel

module PropsMap = Hashtbl.Make(String)

module rec TypeExpr : sig
  type t =
    | Unknown
    | Any
    | Ctor of int * (t list)
    | Class of class_type
    | Function of t list * t
    | Module of module_type
    | Array of t
    | TypeDef of TypeDef.t

  and module_type = {
    export: t;
  }

  and class_type = {
    tcls_extends:    t option;
    tcls_properties: class_property_type list;
    tcls_methods:    function_type list;
  }

  and class_property_type = {
    tcls_property_name: Waterlang_parsing.Identifier.t;
    tcls_property_type: t;
  }

  and function_type = {
    tfun_params: (string * t) list;
    tfun_ret: t;
  }

  (* val pp: Format.formatter -> t -> unit
  val pp_function_type: Format.formatter -> function_type -> unit *)

end = struct

  type t =
    | Unknown
    | Any
    | Ctor of int * (t list)
    | Class of class_type
    | Function of t list * t
    | Module of module_type
    | Array of t
    | TypeDef of TypeDef.t

  and module_type = {
    export: t;
  }

  and class_type = {
    tcls_extends:    t option;
    tcls_properties: class_property_type list;
    tcls_methods:    function_type list;
  }

  and class_property_type = {
    tcls_property_name: Waterlang_parsing.Identifier.t;
    tcls_property_type: t;
  }

  and function_type = {
    tfun_params: (string * t) list;
    tfun_ret: t;
  }

  (* let rec pp formatter ty =
    match ty with
    | Unknown -> Format.pp_print_string formatter "unknown"
    | Any -> Format.pp_print_string formatter "any"
    | Ctor(sym, _) -> Format.fprintf formatter "%d<>" sym
    | Class _ -> Format.pp_print_string formatter "Class"
    | Function _ ->
      Format.pp_print_string formatter "function"

    | Module _ -> Format.pp_print_string formatter "module"

    | Array _ -> Format.pp_print_string formatter "Array"

    | TypeDef _ -> Format.pp_print_string formatter "TypeDef"

  and pp_function_type formatter fun_ =
    Format.pp_print_string formatter "(";

    List.iteri
      ~f:(fun index param ->
        let (name, ty) = param in
        Format.pp_print_string formatter name;
        Format.pp_print_string formatter ": ";
        pp formatter ty;
        if index <> (List.length fun_.tfun_params) - 1 then (
          Format.pp_print_string formatter ", "
        )
      )
      fun_.tfun_params;

    Format.pp_print_string formatter ") => ";
    pp formatter fun_.tfun_ret *)
  
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
  }

  and spec =
    | Primitive
    | Object 
    | Alias of TypeExpr.t
    | Module_ of module_type
    | Enum of enum_type

  and t = {
    builtin: bool;
    name: string;
    spec: spec;
  }

  val name: t -> string

  val builtin: t -> bool

  val create: ?builtin:bool -> string -> spec -> t

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
  }

  and spec =
    | Primitive
    | Object
    | Alias of TypeExpr.t
    | Module_ of module_type
    | Enum of enum_type

  and t = {
    builtin: bool;
    name: string;
    spec: spec;
  }

  let name sym = sym.name

  let builtin sym = sym.builtin

  let create ?(builtin=false)  name spec =
    {
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

let none _ = ()

type node = {
  loc: Waterlang_lex.Loc.t;
  value: TypeExpr.t;
  deps: int list;
  check: int -> unit;
}

let unknown = {
  loc = Waterlang_lex.Loc.none;
  value = TypeExpr.Unknown;
  deps = [];
  check = none;
}
