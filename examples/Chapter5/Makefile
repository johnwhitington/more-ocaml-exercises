# Build the camlpdf library as byte code and native code
SOURCES = examples.ml

RESULT = examples

INCDIRS = ../../ ../Chapter4/

LIBS = input

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../OCamlMakefile

include $(OCAMLMAKEFILE)
