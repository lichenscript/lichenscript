
type numeric_type =
  | Num_u32
  | Num_i32
  | Num_u64
  | Num_i64
  | Num_f32
  | Num_f64

type core_type =
  | Any
  | Numeric of numeric_type
  | String
  | Ctor of Symbol.type_sym
  | Class of class_type

and class_type = {
  tcls_name:       Symbol.type_sym;
  tcls_extends:    Symbol.type_sym option;
  tcls_methods:    class_method_type list;
  tcls_properties: class_property_type list;
}

and class_method_type = {
  tcls_method_name: Symbol.type_sym;
  tcls_method_params: core_type list;
}

and class_property_type = {
  tcls_property_name: Symbol.type_sym;
  tcls_property_type: core_type;
}
