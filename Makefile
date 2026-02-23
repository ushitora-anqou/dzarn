.PHONY: build
build:
	nix fmt .
	dune fmt
	dune build
