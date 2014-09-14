PDFMODS = pdf pdfwrite pdfpage pdfhello

SOURCES = $(foreach x,$(PDFMODS),$(x).ml $(x).mli) pdfmain.ml

MODS = more

INCDIRS = ../../more/

RESULT = pdfmain

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code native-code-library byte-code byte-code-library top htdoc

OCAMLMAKEFILE = ../../more/OCamlMakefile

include $(OCAMLMAKEFILE)

