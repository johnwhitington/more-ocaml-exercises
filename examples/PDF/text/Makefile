# Build the camlpdf library as byte code and native code
SOURCES = text.ml

RESULT = text

INCDIRS = ../../../more/ ../ ../../Chapter4/

LIBS = input pdfmain 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : byte-code top

OCAMLMAKEFILE = ../../../more/OCamlMakefile

include $(OCAMLMAKEFILE)

