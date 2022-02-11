#!/bin/bash

export LSC_RUNTIME="./runtime"
export LSC_STD="./std"

# FLAGS="--base ./examples --platform wasm32"
FLAGS="--base ./examples"

dune build
_build/default/test/lichenscript_test.exe ./examples -C ./_build/default/bin/main.exe $@ \
    -- $FLAGS
