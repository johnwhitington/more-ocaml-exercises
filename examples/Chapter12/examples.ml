open More

type turn = O | X | E

let won [a; b; c; d; e; f; g; h; i] =
  a && b && c || d && e && f || g && h && i || a && d && g ||
  b && e && h || c && f && i || a && e && i || c && e && g

let empty b =
  List.map snd
    (List.filter (fun (t, _) -> t = E)
      (List.combine b [1; 2; 3; 4; 5; 6; 7; 8; 9]))

let replace turn board p =
  Util.take board (p - 1) @ [turn] @ Util.drop board p

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

