# Build the camlpdf library as byte code and native code
SOURCES = examples.ml

RESULT = input

INCDIRS = ../../

LIBS = more 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../OCamlMakefile

include $(OCAMLMAKEFILE)
