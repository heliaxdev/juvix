all: build install

build:
	stack build

install: build
	stack install

lint:
	stack exec -- hlint app src test

test:
	stack test

repl:
	stack ghci

clean:
	stack clean --full

.PHONY: all build install test lint repl clean
