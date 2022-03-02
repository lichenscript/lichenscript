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
open Core_kernel
open Lichenscript_lex
open Lichenscript_parsing
open Lichenscript_typing

type file = {
	path: string;
	ast: Ast.program option;
	typed_env: Env.t;
  typed_tree: Typedtree.program option;
	extern_modules: string list;  (* full id *)

  (* relative_map -> absolute map *)
  imports_map: (string, string) Hashtbl.t;
}

type export = {
  export_name: string;
  export_var: Scope.variable;
}

(* prev export * new export *)
exception ReexportSymbol of (Loc.t * export * export)

type t = {
  mod_full_path: string;
  mutable files: file list;
  exports: (string, export) Hashtbl.t;
  module_scope: Scope.scope;
}

let create ~full_path ~module_scope () =
  {
    mod_full_path = full_path;
    files = [];
    exports = Hashtbl.create (module String);
    module_scope;
  }

let module_scope env = env.module_scope

let add_file env file =
  env.files <- file::env.files

let files env = List.rev env.files

let set_files env files =
  env.files <- files

let finalize_module_exports env =
  let module_scope = env.module_scope in
  let vars = module_scope#vars in
  List.iter
    ~f:(fun (name, ty_var) -> 
      let visibility = module_scope#get_visibility name in
      match visibility with
      | Some Asttypes.Pvisibility_public -> (
        let export = {
          export_name = name;
          export_var = ty_var;
        } in
        (match Hashtbl.find env.exports name with
        | Some old_export -> (
          raise (ReexportSymbol (ty_var.var_loc, old_export, export))
        )
        | None -> ()
        );
        Hashtbl.set env.exports ~key:name ~data:export;
      )

      | _ -> ()
    )
    vars

let find_export env ~name =
  Hashtbl.find env.exports name

let exports env =
  Hashtbl.to_alist env.exports
