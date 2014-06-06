let sum l = List.fold_left ( + ) 0 l

let maxlist l = List.fold_left max min_int l

let all l = List.fold_left ( && ) true l

let any l = List.fold_left ( || ) false l

let concat l = List.fold_left ( @ ) [] l

let rev l = List.fold_left (fun a e -> e :: a) [] l

let member x l = List.fold_left (fun a e -> e = x || a) false l

let setify l = List.fold_left (fun a e -> if List.mem e a then a else e :: a) [] l

let identity l = List.fold_right (fun e a -> e :: a) l []

let append x y = List.fold_right (fun e a -> e :: a) x y

let split l = List.fold_right (fun (x, y) (xs, ys) -> (x :: xs, y :: ys)) l ([], [])

type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t =
  match t with
    Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let example =
  Br (1, Br (2, Lf, Lf), Br (6, Br (4, Lf, Lf), Lf))

let tree_size t = fold_tree (fun _ l r -> 1 + l + r) 0 t

let tree_sum t = fold_tree (fun x l r -> x + l + r) 0 t

let tree_preorder t  = fold_tree (fun x l r -> [x] @ l @ r) [] t

let tree_inorder t   = fold_tree (fun x l r -> l @ [x] @ r) [] t

let tree_postorder t = fold_tree (fun x l r -> l @ r @ [x]) [] t

let map f l = List.fold_right (fun a e -> f e :: a) l

let fold_right f l e =
  List.fold_left (fun x y -> f y x) e (List.rev l)

