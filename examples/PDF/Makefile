# Build the camlpdf library as byte code and native code
PDFMODS = pdf pdfwrite pdfpage pdfhello pdfmain

SOURCES = $(foreach x,$(PDFMODS),$(x).ml $(x).mli)

MODS = more

INCDIRS = ../../

RESULT = pdfmain

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code byte-code-library top htdoc

OCAMLMAKEFILE = ../../OCamlMakefile

include $(OCAMLMAKEFILE)

