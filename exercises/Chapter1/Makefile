# Build the camlpdf library as byte code and native code
SOURCES = exercises.ml

RESULT = exercises

INCDIRS = ../../more/

LIBS = more 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code top

OCAMLMAKEFILE = ../../more/OCamlMakefile

include $(OCAMLMAKEFILE)
