Examples for Chapter 10
=======================

These examples can be explored either by copying-and-pasting into the OCaml
top-level, or by the prepared top level "examples.top":

        OCaml

# open Examples;;
# let rec without x l =
match l with
  [] -> []
| h::t when h = x -> t
| h::t -> h :: without x t

let rec perms l =
  match l with
    [] -> [[]]
  | l ->
      List.concat
        (List.map
          (fun x -> List.map (fun l -> x :: l) (perms (without x l)))
          l);;
val without : 'a -> 'a list -> 'a list = <fun>
val perms : 'a list -> 'a list list = <fun>
# perms [1;2;3];;
- : int list list =
[[1; 2; 3]; [1; 3; 2]; [2; 1; 3]; [2; 3; 1]; [3; 1; 2]; [3; 2; 1]]

