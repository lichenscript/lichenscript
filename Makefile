
hello_world: compiler
	./_build/default/bin/main.exe ./examples/hello_world/main.wt \
		--std ./std --base ./examples -D ./_build_wt

compiler:
	dune build
