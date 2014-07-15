# Build the camlpdf library as byte code and native code
SOURCES = bits.ml

RESULT = bits

INCDIRS = ../Chapter4/

LIBS = input

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code-library top

OCAMLMAKEFILE = ../../more/OCamlMakefile

include $(OCAMLMAKEFILE)
