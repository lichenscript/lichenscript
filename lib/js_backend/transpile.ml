open Lichenscript_lex
open Lichenscript_typing
open Lichenscript_typing.Typedtree
open Core_kernel
open Sourcemap

type t = {
  ctx: Type_context.t;
  buffer: Buffer.t;
  sourcemap: sourcemap_generator;
  mutable line: int;
  mutable col: int;
}

let ps env str =
  let lines = String.split str ~on:'\n' in
  List.iteri
    ~f:(fun index line ->
      Buffer.add_string env.buffer line;
      env.col <- env.col + (String.length line);
      if index >= 1 then (
        env.line <- env.line + 1;
        env.col <- 0;
      )
    )
    lines

let create ~ctx () =
  let sourcemap = new sourcemap_generator in
  let buffer = Buffer.create 1024 in
  {
    ctx;
    buffer;
    sourcemap;
    line = 1;
    col = 0;
  }

let rec tranpile_declaration env delcaration =
  let open Declaration in
  let { spec; _ } = delcaration in
  match spec with
  | Class _ -> ()
  | Function_ _fun ->
    transpile_function env _fun

  | Declare _
  | Enum _
  | Import _ -> ()

and transpile_function env _fun =
  let open Function in
  ps env "function ";
  tranpile_id env _fun.header.name _fun.header.name_loc;
  ps env "() {\n";
  ps env ")\n"

and tranpile_id env (name, _) (loc: Loc.t) =
  env.sourcemap#add_location env.col 0 loc.start.line loc.start.column;
  ps env name

let transpile_program ~ctx declarations =
  let env = create ~ctx () in
  List.iter ~f:(tranpile_declaration env) declarations;
  env
