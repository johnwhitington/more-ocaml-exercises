Examples for Chapter 12
=======================

These examples can be explored either by copying-and-pasting into the OCaml
top-level, or by the prepared top level "examples.top":

        OCaml version

# open Examples;;
# game_tree;;
- : Examples.tree =
Move ([E; E; E; E; E; E; E; E; E],
 [Move ([O; E; E; E; E; E; E; E; E],
   [Move ([O; X; E; E; E; E; E; E; E],
     [Move ([O; X; O; E; E; E; E; E; E],
       [Move ([O; X; O; X; E; E; E; E; E],
         [Move ([O; X; O; X; O; E; E; E; E],
           [Move ([O; X; O; X; O; X; E; E; E],
             [Move ([O; X; O; X; O; X; O; E; E], []);
              Move ([O; X; O; X; O; X; E; O; E],
               [Move ([O; X; O; X; O; X; X; O; E],
                 [Move ([O; X; O; X; O; X; X; O; O], [])]);
                Move ([O; X; O; X; O; X; E; O; X],
                 [Move ([O; X; O; X; O; X; O; O; X], [])])]);
              Move ([O; X; O; X; O; X; E; E; O], [])]);
            Move ([O; X; O; X; O; E; X; E; E],
             [Move ([O; X; O; X; O; O; X; E; E],
               [Move ([O; X; O; X; O; O; X; X; E],
                 [Move ([O; X; O; X; O; O; X; X; O], [])]);
                Move ([O; X; O; X; O; O; X; E; X],
                 [Move ([O; X; O; X; O; O; X; O; X], [])])]);
              Move ([O; X; O; X; O; E; X; O; E],
               [Move ([O; X; O; X; O; X; X; O; E],
                 [Move ([O; X; O; X; O; X; X; O; O], [])]);
                Move ([O; X; O; X; O; E; X; O; X],
                 [Move ([O; X; O; X; O; O; X; O; X], [])])]);
              ...]);
            ...]);
          ...]);
        ...]);
      ...]);
    ...]);
  ...])
# 

