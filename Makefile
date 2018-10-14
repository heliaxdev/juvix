all: build install

build:
	stack build

install: build
	stack install

lint:
	stack exec -- hlint app codegen src test

test:
	stack test

repl-lib:
	stack ghci juvix:lib

repl-exe:
	stack ghci juvix:exe:juvix

repl-codegen:
	stack ghci juvix:exe:idris-codegen-juvix

clean:
	stack clean

clean-full:
	stack clean --full

.PHONY: all build install test lint repl-lib repl-exe repl-codegen clean clean-full
