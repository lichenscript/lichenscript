open Lichenscript_typing

type result = {
  main_function_name: string option;
  declarations: Ir.Decl.t list;
  global_class_init: string option;
}

type config = {
  (* automatic reference counting *)
  arc: bool;
  prepend_lambda: bool;
  ptr_size: Ir.ptr_size option;
}

val transform_declarations: config:config -> Program.t -> Typedtree.Declaration.t list -> result
