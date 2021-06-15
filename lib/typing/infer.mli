open Waterlang_parsing

val infer_class: Typedtree._class -> Core_type.TypeValue.t

val infer: Env.t -> Ast._type -> Core_type.TypeValue.t
