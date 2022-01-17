open Lichenscript_typing

type result = {
  main_function_name: string option;
  declarations: C_op.Decl.t list;
}

val transform_declarations: Type_context.t -> Typedtree.Declaration.t list -> result
