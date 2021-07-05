
let codegen_allocator_facility (module M: Dsl.BinaryenModule) =
  let module Dsl = Dsl.Binaryen(M) in
  let open Dsl in
  i32
