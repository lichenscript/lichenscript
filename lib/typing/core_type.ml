open Waterlang_lex

type kind =
  | Local
  | Global

class type_sym (scope_id: int) (name: string) (kind: kind) =
  object
    method get_name = name

    method get_kind = kind

    method get_scope_id = scope_id

  end

let pp_type_sym formatter (sym: type_sym) =
  Format.pp_print_string formatter sym#get_name

module rec TypeValue : sig
  type t =
    | Unknown
    | Any
    | Unit
    | Ctor of type_sym
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
    tfun_params: t list;
    tfun_ret: t;
  }

  val pp: Format.formatter -> t -> unit

end = struct
  type t =
    | Unknown
    | Any
    | Unit
    | Ctor of type_sym
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
    tfun_params: t list;
    tfun_ret: t;
  }

  let pp formatter = function
  | Unknown -> Format.pp_print_string formatter "unknown"
  | Any -> Format.pp_print_string formatter "any"
  | Unit -> Format.pp_print_string formatter "unit"
  | Ctor sym -> Format.pp_print_string formatter sym#get_name
  | Class _ -> Format.pp_print_string formatter "Class"
  | Function _ -> Format.pp_print_string formatter "Function"
  | Array _ -> Format.pp_print_string formatter "Array"
  
end

and VarSym : sig
  type t = {
    id_in_scope: int;
    name:        string;
    def_type:    TypeValue.t;
    def_loc:     Loc.t option;
    kind:        kind;
    scope_id:    int;
    builtin:     bool;
  }

  val mk_local: id_in_scope:int -> scope_id:int -> string -> t

end = struct
  type t = {
    id_in_scope: int;
    name:        string;
    def_type:    TypeValue.t;
    def_loc:     Loc.t option;
    kind:        kind;
    scope_id:    int;
    builtin:     bool;
  }

  let mk_local ~id_in_scope ~scope_id name =
    {
      id_in_scope;
      name = name;
      def_type = TypeValue.Unknown;
      def_loc = None;
      kind = Local;
      scope_id = scope_id;
      builtin = false;
    }
  
end

class cls_type_sym (scope_id: int) (name: string) (kind: kind) (c: TypeValue.t) =
  object inherit type_sym scope_id name kind
    val cls: TypeValue.t = c
  end

class alias_type_sym (scope_id: int) (name: string) (kind: kind) (alias: TypeValue.t) =
  object inherit type_sym scope_id name kind
    method get_alias = alias
  end
