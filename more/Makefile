# Build the camlpdf library as byte code and native code
SOURCES = more.mli more.ml

RESULT = more

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code-library byte-code-library native-code byte-code top

include OCamlMakefile

