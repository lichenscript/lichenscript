
open Core_kernel
open Core_type
open Typedtree

let infer_class cls =
  let (cls_methods, cls_props) =
    List.partition_map
      ~f:(function
      | Tcls_method _method ->
        `Fst _method

      | Tcls_property prop ->
        `Snd prop

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
