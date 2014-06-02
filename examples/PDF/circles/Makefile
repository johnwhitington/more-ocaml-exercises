# Build the camlpdf library as byte code and native code
SOURCES = circles.ml

RESULT = circles

INCDIRS = ../../../more/ ../

LIBS = pdfmain 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../../more/OCamlMakefile

include $(OCAMLMAKEFILE)

