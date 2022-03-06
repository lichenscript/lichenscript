
FLAGS = --std ./std -R ./runtime --base ./examples 

hello_world: compiler
	rm -rf ./_build_wt/hello_world
	mkdir -p ./_build_wt/hello_world
	./_build/default/bin/main.exe run ./examples/hello_world/main.lc \
		$(FLAGS) -D ./_build_wt/hello_world

release-darwin-x64: compiler
	rm -rf ./npm/lichenscript-darwin-x64/bin
	mkdir -p ./npm/lichenscript-darwin-x64/bin
	cp ./_build/default/bin/main.exe ./npm/lichenscript-darwin-x64/bin/lichenscript

release-darwin-arm64: compiler
	rm -rf ./npm/lichenscript-darwin-arm64/bin
	mkdir -p ./npm/lichenscript-darwin-arm64/bin
	cp ./_build/default/bin/main.exe ./npm/lichenscript-darwin-arm64/bin/lichenscript

release-linux-x64: compiler
	rm -rf ./npm/lichenscript-linux-x64/bin
	mkdir -p ./npm/lichenscript-linux-x64/bin
	cp ./_build/default/bin/main.exe ./npm/lichenscript-linux-x64/bin/lichenscript

compiler:
	dune build

bump:
	./_build/default/npm_version_bumper/npm_version_bumper.exe ./npm --main lichenscript --ml ./bin/version.ml
