# Build the camlpdf library as byte code and native code
SOURCES = examples.ml

RESULT = examples

INCDIRS = ../../ ../Chapter5/ ../Chapter4/

LIBS = input bits

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : byte-code top

OCAMLMAKEFILE = ../../OCamlMakefile

include $(OCAMLMAKEFILE)
