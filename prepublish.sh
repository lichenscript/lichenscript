#!/bin/bash

WRAPPER=./_build/default/js_output_wrapper/js_output_wrapper.exe

rm -rf npm/lichenscript/std
rm -rf npm/lichenscript/runtime

cp -rf ./std npm/lichenscript/std
cp -rf ./runtime npm/lichenscript/runtime

rm -f npm/lichenscript/README.md
cp ./README.md npm/lichenscript/README.md

rm -rf npm/lichenscript-web/dist
mkdir -p npm/lichenscript-web/dist
$WRAPPER _build/default/web/lichenscript_web.bc.js > npm/lichenscript-web/dist/lichenscript_web.bc.js 

rm -f npm/lichenscript-web/README.md
cp ./README.md npm/lichenscript-web/README.md
