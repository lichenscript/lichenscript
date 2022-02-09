
FLAGS = --std ./std -R ./runtime --base ./examples 

hello_world: compiler
	rm -rf ./_build_wt/hello_world
	mkdir -p ./_build_wt/hello_world
	./_build/default/bin/main.exe run ./examples/hello_world/main.lc \
		$(FLAGS) -D ./_build_wt/hello_world

fibonacci: compiler
	rm -rf ./_build_wt/fibonacci
	mkdir -p ./_build_wt/fibonacci
	./_build/default/bin/main.exe run ./examples/fibonacci/main.wt \
		$(FLAGS) -D ./_build_wt/fibonacci

class: compiler
	rm -rf ./_build_wt/class
	mkdir -p ./_build_wt/class
	./_build/default/bin/main.exe run ./examples/class/main.wt \
		$(FLAGS) -D ./_build_wt/class

enum: compiler
	rm -rf ./_build_wt/enum
	mkdir -p ./_build_wt/enum
	./_build/default/bin/main.exe run ./examples/enum/main.wt \
		$(FLAGS) -D ./_build_wt/enum

is_prime: compiler
	rm -rf ./_build_wt/is_prime
	mkdir -p ./_build_wt/is_prime
	./_build/default/bin/main.exe run ./examples/is_prime/main.lc \
		$(FLAGS) -D ./_build_wt/is_prime

release-darwin-x64: compiler
	rm -rf ./npm/lichenscript-darwin-x64/bin
	mkdir -p ./npm/lichenscript-darwin-x64/bin
	cp ./_build/default/bin/main.exe ./npm/lichenscript-darwin-x64/bin/lichenscript

compiler:
	dune build
