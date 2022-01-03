

let codegen tree =
  let env = Codegen.create () in
  Codegen.codegen_program env tree;
  Codegen.contents env
