Examples for Chapter 9
======================

These examples can be explored either by copying-and-pasting into the OCaml
top-level, or by the prepared top level "examples.top". It requires that the
More module is installed for some examples:

        OCaml

# #use "topfind";;
- : unit = ()
Findlib has been successfully loaded. Additional directives:
  #require "package";;      to load a package
  #list;;                   to list the available packages
  #camlp4o;;                to load camlp4 (standard syntax)
  #camlp4r;;                to load camlp4 (revised syntax)
  #predicates "p,q,...";;   to set these predicates
  Topfind.reset();;         to force that packages will be reloaded
  #thread;;                 to enable threads

- : unit = ()
# #require "More";;
/Users/john/.opam/4.01.0/lib/More: added to search path
/Users/john/.opam/4.01.0/lib/More/more.cma: loaded
# open More;;
# open Examples;;
# test ();;
Pattern           String            Expected true   Actual true   Correct true
Pattern a         String            Expected false   Actual false   Correct true
Pattern a         String a          Expected true   Actual true   Correct true
Pattern ab        String aaab       Expected true   Actual true   Correct true
Pattern ab        String b          Expected false   Actual false   Correct true
Pattern ab        String c          Expected false   Actual false   Correct true
Pattern +         String            Expected false   Actual false   Correct true
Pattern +a        String            Expected false   Actual false   Correct true

