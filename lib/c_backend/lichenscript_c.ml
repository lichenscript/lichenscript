

let codegen ~prog ~includes ~init_calls ~ptr_size tree =
  let env = Codegen.codegen_program ~prog ~includes ~init_calls ~ptr_size tree in
  Codegen.contents env
