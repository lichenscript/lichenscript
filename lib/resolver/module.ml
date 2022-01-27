(*
 * This file is part of LichenScript Compiler.
 *
 * LichenScript Compiler is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
 *
 * LichenScript Compiler is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with LichenScript Compiler. If not, see <https://www.gnu.org/licenses/>.
 *)
open Core
open Lichenscript_parsing
open Lichenscript_typing

type file = {
	path: string;
	ast: Ast.program option;
	typed_env: Env.t;
  typed_tree: Typedtree.program option;
	extern_modules: string list;  (* full id *)
}

type export = {
  export_name: string;
  export_var: Scope.variable;
}

(* prev export * new export *)
exception ReexportSymbol of (export * export)

type t = {
  mod_full_path: string;
  mutable files: file list;
  exports: (string, export) Hashtbl.t;
}

let create ~full_path () =
  {
    mod_full_path = full_path;
    files = [];
    exports = Hashtbl.create (module String);
  }

let add_file env file =
  env.files <- file::env.files

let files env = List.rev env.files

let set_files env files =
  env.files <- files

let finalize_module_exports env =
  List.iter
    ~f:(fun file ->
      let typed_env = file.typed_env in
      let module_scope = Env.peek_scope typed_env in
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
              raise (ReexportSymbol (old_export, export))
            )
            | None -> ()
            );
            Hashtbl.set env.exports ~key:name ~data:export;
          )

          | _ -> ()
        )
        vars
    )
    env.files

let find_export env ~name =
  Hashtbl.find env.exports name
