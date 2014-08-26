Examples for Chapter 7
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
# let rec split ?chunksize l =
  let ch =
    match chunksize with None -> 1 | Some x -> x
  in
    try
      Util.take l ch :: split ~chunksize:ch (Util.drop l ch)
    with
      _ -> match l with [] -> [] | _ -> [l];;
val split : ?chunksize:int -> 'a list -> 'a list list = <fun>

