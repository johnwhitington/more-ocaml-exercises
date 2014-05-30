# Build the camlpdf library as byte code and native code
SOURCES = text.ml

RESULT = text

INCDIRS = ../../../ ../ ../../Chapter4/

LIBS = input pdfmain 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : byte-code top

OCAMLMAKEFILE = ../../../OCamlMakefile

include $(OCAMLMAKEFILE)

