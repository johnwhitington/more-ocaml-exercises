# Build the camlpdf library as byte code and native code
SOURCES = compressed.ml

RESULT = compressed

INCDIRS = ../../../more/ ../../../examples/Chapter4/ ../../../examples/Chapter5/ ../../../examples/PDF/ ../../../examples

LIBS = input bits pdfmain 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../../more/OCamlMakefile

include $(OCAMLMAKEFILE)

