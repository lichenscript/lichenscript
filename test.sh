#!/bin/bash

FLAGS="--std ./std -R ./runtime --debug --base ./examples"

dune build
_build/default/test/lichenscript_test.exe ./examples -C ./_build/default/bin/main.exe $@ \
    -- $FLAGS
