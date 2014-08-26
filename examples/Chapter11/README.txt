Examples for Chapter 11
=======================

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
# #require "more";;
/Users/john/.opam/4.01.0/lib/more: added to search path
/Users/john/.opam/4.01.0/lib/more/more.cma: loaded
# open Examples;;
# open More;;
# let rand = Array.to_list (Array.init 50000 (fun _ -> Random.int 1073741823))
  ;;
val rand : int list =
  [101034467; 407825131; 571293937; 506820152; 513357035; 133347790;
   531841703; 851891377; 602104092; 877631873; 250715754; 803796040;
   250979474; 13565389; 957676818; 233362745; 643922655; 470675292;
   216495966; 754417939; 148578049; 625049537; 1049736837; 344341571;
   131656454; 4065002; 192074147; 928914772; 998912359; 935291503; 805432088;
   1052323739; 640454764; 676817165; 540149770; 1002922518; 887285858;
   186280394; 47907969; 1044161925; 776929507; 1012249136; 648339534;
   1063591659; 435129950; 1048695423; 558080928; 400827425; 868668333;
   504345483; 126294874; 721355293; 769601702; 534288566; 813186100;
   232690918; 411890129; 763368067; 361993099; 438527563; 1068639292;
   263531968; 830473283; 168817017; 480707240; 790865527; 732976757;
   64523511; 199845783; 1005584783; 203782852; 347110319; 409182594;
   864835494; 744267760; 583708003; 600003154; 534075962; 745169002;
   1000324786; 508410485; 318369024; 576528236; 694772256; 395838250; ...

