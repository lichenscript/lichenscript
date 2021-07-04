
open Core_kernel
open Core_type
open Waterlang_parsing
open Typedtree

let infer_class cls =
  let (cls_methods, cls_props) =
    List.partition_map
      ~f:(function
      | Tcls_method _method ->
        First _method

      | Tcls_property prop ->
        Second prop

      )
    cls.tcls_body.tcls_body_elements
  in

  let tcls_properties: TypeValue.class_property_type list =
    List.map
    ~f:(fun prop -> { TypeValue.
        tcls_property_name = prop.tcls_property_name;
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

let rec infer env (ty: Ast._type) =
  let open Ast in
  let { pty_desc; pty_loc } = ty in
  match pty_desc with
  | Pty_any -> TypeValue.Any
  | Pty_var _ -> failwith "unreachable"
  | Pty_ctor(id, _) ->
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
          loc = pty_loc;
        };
        TypeValue.Unknown
    end

  | Pty_arrow(params, ret) ->
    let params_types = List.map ~f:(infer env) params in
    let params_ret = infer env ret in
    TypeValue.(Function {
      tfun_params = params_types;
      tfun_ret = params_ret;
    })
