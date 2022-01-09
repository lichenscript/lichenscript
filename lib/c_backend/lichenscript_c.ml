

let codegen ~ctx tree =
  let env = Codegen.create ~ctx () in
  Codegen.codegen_program env tree;
  Codegen.contents env
