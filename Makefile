
hello_world: compiler
	rm -rf ./_build_wt/hello_world
	mkdir -p ./_build_wt/hello_world
	./_build/default/bin/main.exe run ./examples/hello_world/main.wt \
		--std ./std --base ./examples -D ./_build_wt/hello_world

fibonacci: compiler
	rm -rf ./_build_wt/fibonacci
	mkdir -p ./_build_wt/fibonacci
	./_build/default/bin/main.exe run ./examples/fibonacci/main.wt \
		--std ./std --base ./examples -D ./_build_wt/fibonacci

class: compiler
	rm -rf ./_build_wt/class
	mkdir -p ./_build_wt/class
	./_build/default/bin/main.exe build ./examples/class/main.wt \
		--std ./std --base ./examples -D ./_build_wt/class

compiler:
	dune build
