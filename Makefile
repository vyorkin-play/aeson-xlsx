test: build
	stack test

cli: build
	stack exec cli

sandbox: build
	stack exec sandbox -- ./examples/bar.xlsx

build:
	stack build
