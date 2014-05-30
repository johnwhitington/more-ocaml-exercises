open More

type turn = O | X | E

let won [a; b; c; d; e; f; g; h; i] =
  a && b && c || d && e && f || g && h && i || a && d && g ||
  b && e && h || c && f && i || a && e && i || c && e && g

let replace turn board p =
  Util.take board (p - 1) @ [turn] @ Util.drop board p

let empty b =
  List.map snd (List.filter (fun (t, _) -> t = E) (List.combine b [1; 2; 3; 4; 5; 6; 7; 8; 9]))

let flipturn t =
  match t with O -> X | X -> O

type tree = Move of turn list * tree list

let rec nextmoves turn board =
  let next =
    if won (List.map (fun t -> t = O) board) || won (List.map (fun t -> t = X) board) then
      []
    else
      List.map
        (nextmoves (flipturn turn))
        (List.map (replace turn board) (empty board))
  in
    Move (board, next)

let game_tree =
  nextmoves O [E; E; E; E; E; E; E; E; E]

let rec numwins turn (Move (b, bs)) =
  (if won (List.map (fun t -> t = turn) b) then 1 else 0) +
  List.fold_left ( + ) 0 (List.map (numwins turn) bs)

