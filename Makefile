.PHONY: test
test:
	$(MAKE) build
	dune runtest

.PHONY: build
build:
	nix fmt .
	dune fmt
	dune build
