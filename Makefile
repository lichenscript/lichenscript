
compiler:
	dune build

hello_world: compiler
	./_build/default/bin/main.exe ./examples/hello_world/main.wt --std ./std
