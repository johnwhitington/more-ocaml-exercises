# Build the camlpdf library as byte code and native code
SOURCES = input.ml text.ml

RESULT = text

INCDIRS = ../../../more/ ../../../examples/pdf/ ../../../examples

LIBS = more pdfmain 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../../more/OCamlMakefile

include $(OCAMLMAKEFILE)

