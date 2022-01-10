
hello_world: compiler
	rm -rf ./_build_wt/hello_world
	mkdir -p ./_build_wt/hello_world
	./_build/default/bin/main.exe build ./examples/hello_world/main.wt \
		--std ./std --base ./examples -D ./_build_wt/hello_world

fibonacci: compiler
	rm -rf ./_build_wt/fibonacci
	mkdir -p ./_build_wt/fibonacci
	./_build/default/bin/main.exe build ./examples/fibonacci/main.wt \
		--std ./std --base ./examples -D ./_build_wt/fibonacci

compiler:
	dune build
