# Build the camlpdf library as byte code and native code
SOURCES = input.ml

RESULT = input

INCDIRS = ../../more/

LIBS = more 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code byte-code-library native-code-library top

OCAMLMAKEFILE = ../../more/OCamlMakefile

include $(OCAMLMAKEFILE)
