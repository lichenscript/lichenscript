(*
 * Copyright 2022 Vincent Chan
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)
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

  | Interface _
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
