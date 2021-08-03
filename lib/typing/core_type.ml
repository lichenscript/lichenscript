open Waterlang_lex
open Core_kernel

module PropsMap = Hashtbl.Make(String)

module rec TypeValue : sig
  type t =
    | Unknown
    | Any
    | Unit
    | Ctor of TypeSym.t * (t list)
    | Class of class_type
    | Function of function_type
    | Array of t

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

  val pp: Format.formatter -> t -> unit
  val pp_function_type: Format.formatter -> function_type -> unit

end = struct
  type t =
    | Unknown
    | Any
    | Unit
    | Ctor of TypeSym.t * (t list)
    | Class of class_type
    | Function of function_type
    | Array of t

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

  let rec pp formatter = function
  | Unknown -> Format.pp_print_string formatter "unknown"
  | Any -> Format.pp_print_string formatter "any"
  | Unit -> Format.pp_print_string formatter "unit"
  | Ctor(sym, []) -> Format.fprintf formatter "%s" (TypeSym.name sym)
  | Ctor(sym, args) ->
    begin
      let args_content =
        List.foldi
          ~init:""
          ~f:(fun index acc arg ->
            let suffix = if phys_equal index ((List.length args) - 1) then "" else ", " in
            let arg_content = Format.asprintf "%a" pp arg in
            acc ^ arg_content ^ suffix
          )
          args
      in
      Format.fprintf formatter "%s<%s>" (TypeSym.name sym) args_content
    end

  | Class _ -> Format.pp_print_string formatter "Class"
  | Function fun_ ->
    pp_function_type formatter fun_

  | Array _ -> Format.pp_print_string formatter "Array"

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
    pp formatter fun_.tfun_ret
  
end

and TypeSym : sig
  type module_type = {
    props: TypeValue.t PropsMap.t;
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
    | Alias of TypeValue.t
    | Module_ of module_type
    | Enum of enum_type

  and t = {
    builtin: bool;
    scope_id: int;
    name: string;
    spec: spec;
  }

  val name: t -> string

  val builtin: t -> bool

  val create: ?builtin:bool -> scope_id:int -> string -> spec -> t

  val create_module: unit -> module_type

  val pp: Format.formatter -> t -> unit

end = struct
  type module_type = {
    props: TypeValue.t PropsMap.t;
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
    | Alias of TypeValue.t
    | Module_ of module_type
    | Enum of enum_type

  and t = {
    builtin: bool;
    scope_id: int;
    name: string;
    spec: spec;
  }

  let name sym = sym.name

  let builtin sym = sym.builtin

  let create ?(builtin=false)  ~scope_id name spec =
    {
      builtin;
      scope_id;
      name;
      spec;
    }

  let create_module () =
    {
      props = PropsMap.create ();
    }

  let pp formatter (sym: t) =
    Format.pp_print_string formatter sym.name
  
end

and VarSym : sig

  type enum_field = {
    enum_id: int;
  }

  type spec =
  | Internal
  | ExternalMethod of string * string
  | Enum of enum_field PropsMap.t

  and t = {
    id_in_scope: int;
    name:        string;
    mutable def_type:    TypeValue.t;
    def_loc:     Loc.t option;
    scope_id:    int;
    builtin:     bool;
    spec:        spec;
  }

  val mk_local: id_in_scope:int -> scope_id:int -> ?spec:spec -> string -> t

  val set_def_type: t -> TypeValue.t -> unit

  val pp: Format.formatter -> t -> unit

end = struct
  type enum_field = {
    enum_id: int;
  }

  type spec =
  | Internal
  | ExternalMethod of string * string
  | Enum of enum_field PropsMap.t

  and t = {
    id_in_scope: int;
    name:        string;
    mutable def_type:    TypeValue.t;
    def_loc:     Loc.t option;
    scope_id:    int;
    builtin:     bool;
    spec:        spec;
  }

  let mk_local ~id_in_scope ~scope_id ?(spec=Internal) name =
    {
      id_in_scope;
      name = name;
      def_type = TypeValue.Unknown;
      def_loc = None;
      scope_id = scope_id;
      builtin = false;
      spec;
    }

  let set_def_type t ty =
    t.def_type <- ty

  let pp formatter sym =
    Format.fprintf formatter "VarSym('%s')" sym.name
  
end
