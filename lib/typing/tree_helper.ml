open Core_kernel
open Lichenscript_parsing

let add_top_level_symbols_to_typed_env typed_env tree =
  let { Ast. pprogram_top_level; _ } = tree in
  Hashtbl.iteri
    ~f:(fun ~key ~data ->
      let open Core_type in
      let node = {
        value = TypeExpr.Unknown;
        loc = Lichenscript_lex.Loc.none;
        deps = [];
        check = none;
      } in
      let new_id = Type_context.new_id (Env.ctx typed_env) node in
      let module_scope = (Env.peek_scope typed_env) in
      module_scope#insert_var_symbol key { var_id = new_id; var_kind = Ast.Pvar_const };
      module_scope#insert_type_symbol key new_id;
      module_scope#set_visibility key data;
    )
    pprogram_top_level.names
