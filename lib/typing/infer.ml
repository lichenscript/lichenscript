
(* open Core_kernel
open Core_type
open Waterlang_parsing

let infer_class (cls: Typedtree.Statement._class) =
  let open Typedtree in
  let (cls_methods, cls_props) =
    List.partition_map
      ~f:(function
      | Statement.Cls_method _method ->
        First _method

      | Statement.Cls_property prop ->
        Second prop

      )
    cls.cls_body.cls_body_elements
  in

  let tcls_properties: TypeValue.class_property_type list =
    List.map
    ~f:(fun prop -> { TypeValue.
        tcls_property_name = prop.cls_property_name;
        tcls_property_type = Unknown;
    })
    cls_props
  in

  let tcls_methods: TypeValue.function_type list =
    List.map
    ~f:(function _f -> { TypeValue.
      tfun_params = [];
      tfun_ret= Unknown;
    })
    cls_methods
  in

  TypeValue.(Class {
    tcls_extends = None;
    tcls_properties;
    tcls_methods;
  })

let rec infer env (ty: Ast.Type.t) =
  let open Ast.Type in
  let { spec; loc } = ty in
  match spec with
  | Ty_any -> TypeValue.Any
  | Ty_var _ -> failwith "unreachable"
  | Ty_ctor(id, _) ->
    begin
      let sym = env
        |> Env.peek_scope
        |> (fun scope -> Scope.find_type_symbol scope id.pident_name)
      in
      match sym with
      | Some sym -> TypeValue.Ctor sym
      | None ->
        Env.add_error env { Type_error.
          spec = CannotFindName id.pident_name;
          loc;
        };
        TypeValue.Unknown
    end

  | Ty_arrow(params, ret) ->
    let params_types = List.map ~f:(fun ty -> ("<unknown>", infer env ty)) params in
    let params_ret = infer env ret in
    TypeValue.(Function {
      tfun_params = params_types;
      tfun_ret = params_ret;
    }) *)
