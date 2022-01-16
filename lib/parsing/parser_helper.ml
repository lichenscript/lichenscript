open Lichenscript_lex

(*
 * Two types may match this case:
 * 1. init
 *    a {
 *      key: value,
 *    }
 *
 * 2. match
 *   match a {
 *     pat => clause,
 *   }
 *)

type relaxed_kv_node = {
  relaxed_case_prefix: bool;
  relaxed_key: Ast.Pattern.t;
  relaxed_op: Token.t;
  relaxed_value: Ast.Expression.t;
  relaxed_kv_loc: Loc.t;
}

type relaxed_kv_entry =
  | Relaxed_node of relaxed_kv_node
  | Relaxed_spread of (Loc.t * Ast.Expression.t)

type relaxed_kv_block = {
  entries: relaxed_kv_entry list;
  loc: Loc.t;
}

let cast_relaxed_block_into_match ~start_loc expr (block: relaxed_kv_block) =
  let open Ast.Expression in
  let match_clauses =
    List.map
    (fun entry ->
      match entry with
      | Relaxed_node node -> (
        if not node.relaxed_case_prefix then (
          let err = { Parse_error. perr_spec = MatchClauseNotPrefixCase; perr_loc = node.relaxed_kv_loc } in
          raise (Parse_error.Error [err])
        );
        let clause_pat = node.relaxed_key in
        if node.relaxed_op <> Token.T_ARROW then (
          let spec = Parser_env.get_unexpected_error ~expected:"=>" node.relaxed_op in
          let err = { Parse_error. perr_spec = spec; perr_loc = node.relaxed_kv_loc } in
          raise (Parse_error.Error [err])
        );
        let clause_consequent = node.relaxed_value in
        let clause_loc = node.relaxed_kv_loc in
        {
          clause_pat;
          clause_consequent;
          clause_loc;
        }
      )
      | Relaxed_spread (perr_loc, _) -> (
        let perr_spec = Parser_env.get_unexpected_error Token.T_ELLIPSIS in
        let err = {
          Parse_error.
          perr_loc;
          perr_spec;
        } in
        raise (Parse_error.Error [err])
      )
    )
    block.entries
  in
  Match {
    match_expr = expr;
    match_clauses;
    match_loc = Loc.btwn start_loc block.loc;
  }

let cast_relaxed_block_into_init ~start_loc ~init_name (block: relaxed_kv_block) =
  let open Ast.Expression in
  let init_elements =
    List.map
    (fun entry ->
      match entry with
      | Relaxed_spread (_, spread) ->
        InitSpread spread
      | Relaxed_node node -> (
        let { relaxed_key; relaxed_kv_loc; relaxed_value; _ } = node in
        let open Ast.Pattern in
        let init_entry_key =
          match relaxed_key.spec with
          | Identifier id -> id
          | _ -> failwith "unexpected"
        in
        InitEntry {
          init_entry_key;
          init_entry_loc = relaxed_kv_loc;
          init_entry_value = Some relaxed_value;
        }
      )
    )
    block.entries
  in
  let init = {
    init_name;
    init_loc = Loc.btwn start_loc block.loc;
    init_elements;
  } in
  {
    spec = Init init;
    loc = Loc.btwn start_loc block.loc;
    attributes = []
  }
