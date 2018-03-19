test: build
	stack test

cli: build
	stack exec cli

sandbox: build
	stack exec sandbox -- ./examples/static1.xlsx

build:
	stack build
