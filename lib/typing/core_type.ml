open Waterlang_lex

type kind =
  | Local
  | Global

class type_sym (scope_id: int) (name: string) (kind: kind) =
  object
    method get_name = name

    method get_kind = kind

    method get_scope_id = scope_id

    method builtin = false

  end

class builtin_sym (scope_id: int) (name: string) (kind: kind) =
  object inherit type_sym scope_id name kind

    method! builtin = true

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
    tfun_params: (string * t) list;
    tfun_ret: t;
  }

  let rec pp formatter = function
  | Unknown -> Format.pp_print_string formatter "unknown"
  | Any -> Format.pp_print_string formatter "any"
  | Unit -> Format.pp_print_string formatter "unit"
  | Ctor sym -> Format.pp_print_string formatter sym#get_name
  | Class _ -> Format.pp_print_string formatter "Class"
  | Function fun_ ->
    pp_function_type formatter fun_

  | Array _ -> Format.pp_print_string formatter "Array"

  and pp_function_type formatter fun_ =
    Format.pp_print_string formatter "(";

    List.iteri
      (fun index param ->
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

and VarSym : sig
  type t = {
    id_in_scope: int;
    name:        string;
    mutable def_type:    TypeValue.t;
    def_loc:     Loc.t option;
    kind:        kind;
    scope_id:    int;
    builtin:     bool;
  }

  val mk_local: id_in_scope:int -> scope_id:int -> string -> t

  val set_def_type: t -> TypeValue.t -> unit

end = struct
  type t = {
    id_in_scope: int;
    name:        string;
    mutable def_type:    TypeValue.t;
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

  let set_def_type t ty =
    t.def_type <- ty
  
end

class cls_type_sym (scope_id: int) (name: string) (kind: kind) (c: TypeValue.t) =
  object inherit type_sym scope_id name kind
    val cls: TypeValue.t = c
  end

class alias_type_sym (scope_id: int) (name: string) (kind: kind) (alias: TypeValue.t) =
  object inherit type_sym scope_id name kind
    method get_alias = alias
  end
