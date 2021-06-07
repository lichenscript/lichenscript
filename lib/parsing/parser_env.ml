
open Waterlang_lex

module Loc = Waterlang_lex.Loc

module Lookahead : sig
  type t

  val create : Lex_env.t -> t

  val peek : t -> int -> Lex_result.t

  val lex_env : t -> int -> Lex_env.t

  val junk : t -> unit

end = struct
  type t = {
    mutable la_results: (Lex_env.t * Lex_result.t) option array;
    mutable la_num_lexed: int;
    mutable la_lex_env: Lex_env.t;
  }

  let create lex_env =
    let lex_env = Lex_env.clone lex_env in
    { 
      la_results = [||];
      la_num_lexed = 0;
      la_lex_env = lex_env;
    }

    let next_power_of_two n =
      let rec f i =
        if i >= n then
          i
        else
          f (i * 2)
      in
      f 1
  
    (* resize the tokens array to have at least n elements *)
    let grow t n =
      if Array.length t.la_results < n then
        let new_size = next_power_of_two n in
        let filler i =
          if i < Array.length t.la_results then
            t.la_results.(i)
          else
            None
        in
        let new_arr = Array.init new_size filler in
        t.la_results <- new_arr
  
    (* precondition: there is enough room in t.la_results for the result *)
    let lex t =
      let lex_env = t.la_lex_env in
      let (lex_env, lex_result) =
        Lex.wrapped_token lex_env
      in
      let cloned_env = Lex_env.clone lex_env in
      t.la_lex_env <- lex_env;
      t.la_results.(t.la_num_lexed) <- Some (cloned_env, lex_result);
      t.la_num_lexed <- t.la_num_lexed + 1
  
    let lex_until t i =
      grow t (i + 1);
      while t.la_num_lexed <= i do
        lex t
      done
  
    let peek t i =
      lex_until t i;
      match t.la_results.(i) with
      | Some (_, result) -> result
      (* only happens if there is a defect in the lookahead module *)
      | None -> failwith "Lookahead.peek failed"
  
    let lex_env t i =
      lex_until t i;
      match t.la_results.(i) with
      | Some (lex_env, _) -> lex_env
      (* only happens if there is a defect in the lookahead module *)
      | None -> failwith "Lookahead.peek failed"
  
    (* Throws away the first peeked-at token, shifting any subsequent tokens up *)
    let junk t =
      lex_until t 0;
      if t.la_num_lexed > 1 then Array.blit t.la_results 1 t.la_results 0 (t.la_num_lexed - 1);
      t.la_results.(t.la_num_lexed - 1) <- None;
      t.la_num_lexed <- t.la_num_lexed - 1
  
end

let maximum_lookahead = 2

type token_sink_result = {
  token_loc: Loc.t;
  token: Token.t;
}

type env = {
  errors: Parse_error.t list ref;
  comments: Loc.t Comment.t list ref;
  error_callback: (env -> Parse_error.t -> unit) option;
  lex_env: Lex_env.t ref;
  last_lex_result: Lex_result.t option ref;
  in_function: bool;
  lookahead: Lookahead.t ref;
  token_sink: (token_sink_result -> unit) option ref;
  source: File_key.t option;
}

let lookahead ~i env =
  assert (i < maximum_lookahead);
  Lookahead.peek !(env.lookahead) i

let init_env source content =
  let (lb, errors) =
  try (Sedlexing.Utf8.from_string content, [])
    with Sedlexing.MalFormed ->
      (Sedlexing.Utf8.from_string "", [
        { Parse_error. perr_loc = Loc.none; perr_spec = Parse_error.MalformedUnicode}
      ])
  in
  let lex_env = Lex_env.new_lex_env source lb ~enable_types_in_comments:true in
  {
    errors = ref errors;
    comments = ref [];
    error_callback = None;
    lex_env = ref lex_env;
    last_lex_result = ref None;
    in_function = false;
    source;
    lookahead = ref (Lookahead.create lex_env);
    token_sink = ref None;
  }

let in_function env = env.in_function

let source env = env.source

let errors env = !(env.errors)

module Peek = struct
  open Waterlang_lex

  let ith_token ~i env = Lex_result.token (lookahead ~i env)

  let ith_loc ~i env = Lex_result.loc (lookahead ~i env)

  let ith_errors ~i env = Lex_result.error (lookahead ~i env)

  let ith_lex_env ~i env = Lookahead.lex_env !(env.lookahead) i

  let token env = ith_token ~i:0 env

  let lex_error_to_parse err =
    let (loc, lex_err) = err in
    { Parse_error. perr_loc = loc; perr_spec = Parse_error.LexError lex_err; }

  let errors env =
    let tmp_result = ith_errors ~i:0 env in
    List.map lex_error_to_parse tmp_result

  let loc env = ith_loc ~i:0 env

  let lex_env env = ith_lex_env ~i:0 env
  
end


(* mutators: *)
let error_at env err =
  env.errors := err :: !(env.errors);
  match env.error_callback with
  | None -> ()
  | Some callback -> callback env err

(* other helper functions: *)
let error_list env = List.iter (error_at env)

module Eat = struct

  (* Consume a single token *)
  let token env =
    (* If there's a token_sink, emit the lexed token before moving forward *)
    (match !(env.token_sink) with
    | None -> ()
    | Some token_sink ->
      token_sink
        {
          token_loc = Peek.loc env;
          token = Peek.token env;
        });

    env.lex_env := Peek.lex_env env;

    error_list env (Peek.errors env);
    env.comments := List.rev_append (Lex_result.comments (lookahead ~i:0 env)) !(env.comments);
    env.last_lex_result := Some (lookahead ~i:0 env);

    Lookahead.junk !(env.lookahead)

    (** [maybe env t] eats the next token and returns [true] if it is [t], else return [false] *)
  let maybe env t =
    if Peek.token env = t then (
      token env;
      true
    ) else
      false
  
end

let error env e =
  let loc = Peek.loc env in
  error_at env { Parse_error. perr_loc = loc; perr_spec = e; }

let get_unexpected_error ?expected token =
  let unexpected = Token.explanation_of_token token in
  match expected with
  | Some expected_msg -> Parse_error.LexError (Lex_error.UnexpectedWithExpected (unexpected, expected_msg))
  | None -> Parse_error.LexError (Lex_error.Unexpected unexpected)

let error_unexpected ?expected env =
  (* So normally we consume the lookahead lex result when Eat.token calls
   * Parser_env.advance, which will add any lexing errors to our list of errors.
   * However, raising an unexpected error for a lookahead is kind of like
   * consuming that token, so we should process any lexing errors before
   * complaining about the unexpected token *)
  error_list env (Peek.errors env);
  error env (get_unexpected_error ?expected (Peek.token env))

let last_loc env =
  match !(env.last_lex_result) with
  | Some lex_result -> Some (Lex_result.loc lex_result)
  | None -> None

module Expect = struct

  let error env t =
    let expected = Token.explanation_of_token ~use_article:true t in
    error_unexpected ~expected env

  let token env t =
    if Peek.token env <> t then error env t;
    Eat.token env

  (** [token_opt env T_FOO] eats a token if it is [T_FOO], and errors without consuming if not.
    This differs from [token], which always consumes. Only use [token_opt] when it's ok for
    the parser to not advance, like if you are guaranteed that something else has eaten a
    token. *)
  let token_opt env t =
    if Peek.token env <> t then
      error env t
    else
      Eat.token env

  let identifier env name =
    let t = Peek.token env in
    begin
      match t with
      | Token.T_IDENTIFIER { raw; _ } when raw = name -> ()
      | _ ->
        let expected = Printf.sprintf "the identifier `%s`" name in
        error_unexpected ~expected env
    end;
    Eat.token env
  
end
