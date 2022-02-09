#!/bin/bash

rm -rf npm/lichenscript/std
rm -rf npm/lichenscript/runtime

cp -rf ./std npm/lichenscript/std
cp -rf ./runtime npm/lichenscript/runtime

rm -f npm/lichenscript/README.md
cp ./README.md npm/lichenscript/README.md
