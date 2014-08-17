open More

(* 1 *)

type turn = O | X | E

let won [a; b; c; d; e; f; g; h; i] =
  a && b && c || d && e && f || g && h && i || a && d && g ||
  b && e && h || c && f && i || a && e && i || c && e && g

let replace turn board p =
  Util.take board (p - 1) @ [turn] @ Util.drop board p

let empty b =
  List.map snd (List.filter (fun (t, _) -> t = E) (List.combine b [1; 2; 3; 4; 5; 6; 7; 8; 9]))

let flip_turn t =
  match t with O -> X | X -> O

type tree = Move of turn list * tree list

let rec next_moves turn board =
  let next =
    if
      won (List.map (fun t -> t = O) board) ||
      won (List.map (fun t -> t = X) board)
    then
      []
    else
      List.map
        (next_moves (flip_turn turn))
        (List.map (replace turn board) (empty board))
  in
    Move (board, next)

let game_tree =
  next_moves O [E; E; E; E; E; E; E; E; E]

let rec num_wins turn (Move (b, bs)) =
  (if won (List.map (fun t -> t = turn) b) then 1 else 0) +
  List.fold_left ( + ) 0 (List.map (num_wins turn) bs)

(* num_wins X game_tree = 77904 *)
(* num_wins O game_tree = 131184 *)

let rec drawn (Move (b, bs)) =
    (if
       empty b = [] &&
       not (won (List.map (( = ) O) b)) &&
       not (won (List.map (( = ) X) b))
     then 1 else 0)
  +
    List.fold_left ( + ) 0 (List.map drawn bs)

let rec terminals (Move (b, bs)) =
  (if bs = [] then 1 else 0) +
  List.fold_left ( + ) 0 (List.map terminals bs)

(* drawn game_tree = 46080 *)
(* so total games = 46080 + 77904 + 131184 = 255168 *)

(* 2 *)

(* The changes are simple *)

(* We call this tree2 since no two types in a single file may have the same name *)

type tree2 = Move2 of turn list * (unit -> tree2 list)

let rec next_moves turn board =
  let next =
    fun () ->
      if
        won (List.map (fun t -> t = O) board) ||
        won (List.map (fun t -> t = X) board)
      then
        []
      else
        List.map
          (next_moves (flip_turn turn))
          (List.map (replace turn board) (empty board))
  in
    Move2 (board, next)

let game_tree =
  next_moves O [E; E; E; E; E; E; E; E; E]

(* We wish to force evaluation once, take only the case where the centre is
 * chosen, and then force all of that case. *)
let select_case board (Move2 (_, f)) =
  match List.filter (fun (Move2 (b, _)) -> b = board) (f ()) with
    [Move2 (b, g)] -> g ()
  | _ -> raise (Failure "select_case")

let rec num_wins turn (Move2 (b, bs)) =
  (if won (List.map (fun t -> t = turn) b) then 1 else 0) +
  List.fold_left ( + ) 0 (List.map (num_wins turn) (bs ()))

let pos_wins turn pos =
  List.fold_left ( + ) 0
    (List.map (num_wins turn) (select_case pos game_tree))

let rec drawn (Move2 (b, bs)) =
    (if
       empty b = [] &&
       not (won (List.map (fun t -> t = O) b)) &&
       not (won (List.map (fun t -> t = X) b))
     then 1 else 0)
  +
    List.fold_left ( + ) 0 (List.map drawn (bs ()))

let draws pos =
  List.fold_left ( + ) 0
    (List.map drawn (select_case pos game_tree))

let centre = [E; E; E; E; O; E; E; E; E] 

let side = [E; O; E; E; E; E; E; E; E]

let corner = [O; E; E; E; E; E; E; E; E]

let centre_x_wins = pos_wins X centre

let centre_o_wins = pos_wins O centre

let centre_drawn = draws centre

let side_x_wins = pos_wins X side * 4

let side_o_wins = pos_wins O side * 4

let side_drawn = draws side * 4

let corner_x_wins = pos_wins X corner * 4

let corner_o_wins = pos_wins O corner * 4

let corner_drawn = draws side * 4

(*val centre_x_wins : int = 5616
val centre_o_wins : int = 15648
val centre_drawn : int = 4608
val side_x_wins : int = 40704
val side_o_wins : int = 56928
val side_drawn : int = 20736
val corner_x_wins : int = 31584
val corner_o_wins : int = 58608
val corner_drawn : int = 20736*)

(* total is 255168, of course. *)

(* 3 *)
let rec combinations l =
  match l with
    [] -> [[]]
  | h::t ->
      let cs = combinations t in
        List.map (fun x -> h :: x) cs @ cs

(* We name this tree3 since no two types in a single file may share a name *)
type tree3 = Move of int list * int list * tree3 list

let sum l = List.fold_left ( + ) 0 l = 15

let threes l = List.filter (fun l -> List.length l = 3) (combinations l)

let won l = List.mem true (List.map sum (threes l))
  
let drawn l l' = List.length l + List.length l' = 9

let possibles all =
  List.filter (fun x -> not (List.mem x all)) [1; 2; 3; 4; 5; 6; 7; 8; 9]

let rec next_moves xs os o_is_playing =
  let next =
    if won xs || won os || drawn xs os then [] else
      if o_is_playing
        then
          List.map
            (fun new_os -> next_moves xs new_os (not o_is_playing))
            (List.map (fun q -> q :: os) (possibles (xs @ os)))
        else
          List.map
            (fun new_xs -> next_moves new_xs os (not o_is_playing))
            (List.map (fun q -> q :: xs) (possibles (xs @ os)))
  in
    Move (xs, os, next)

let game_tree = next_moves [] [] true

let rec xwins (Move (xs, os, cs)) =
  (if won xs then 1 else 0) +
  List.fold_left ( + ) 0 (List.map xwins cs)

