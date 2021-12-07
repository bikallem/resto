all:
	dune build

test:
	dune runtest

doc-html:
	dune build @doc

clean:
	dune clean

fmt:
	dune b @fmt --auto-promote

.PHONY: all test doc-html clean fmt
