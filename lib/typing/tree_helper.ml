open Core_kernel
open Lichenscript_parsing

let add_top_level_symbols_to_typed_env typed_env tree =
  let { Ast. pprogram_top_level; _ } = tree in
  Hashtbl.iter_keys
    ~f:(fun key ->
      let open Core_type in
      let node = {
        value = TypeExpr.Unknown;
        loc = Lichenscript_lex.Loc.none;
        deps = [];
        check = none;
      } in
      let new_id = Type_context.new_id (Env.ctx typed_env) node in
      (Env.peek_scope typed_env)#insert_var_symbol key new_id
    )
    pprogram_top_level.names
