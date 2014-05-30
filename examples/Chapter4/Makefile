# Build the camlpdf library as byte code and native code
SOURCES = input.ml

RESULT = input

INCDIRS = ../../

LIBS = more 

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code byte-code-library native-code-library top

OCAMLMAKEFILE = ../../OCamlMakefile

include $(OCAMLMAKEFILE)
