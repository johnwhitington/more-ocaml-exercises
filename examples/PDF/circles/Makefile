# Build the camlpdf library as byte code and native code
SOURCES = circles.ml

RESULT = circles

INCDIRS = ../../../ ../

LIBS = pdfmain 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../../OCamlMakefile

include $(OCAMLMAKEFILE)

