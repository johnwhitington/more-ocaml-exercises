open More

(* 1 *)
let deduct budget expenses =
  List.fold_left ( - ) budget expenses

let deduct = List.fold_left ( - )

(* 2 *)
let length l =
  List.fold_left (fun a _ -> a + 1) 0 l

(* 3 *)
let last exemplar l =
  match l with
    [] -> None
  | _ -> Some (List.fold_left (fun _ e -> e) exemplar l)

(* 4 *)
let rev l =
  List.fold_left (fun a e -> e :: a) [] l

(* 5 *)
let member x l =
  List.fold_left (fun a e -> e = x || a) false l

(* 6 *)
let sentence words =
  List.fold_left (fun a e -> if a = "" then e else a ^ " " ^ e) "" words

(* 7 *)
type 'a tree =
    Lf
  | Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t =
  match t with
    Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let max_depth l =
  fold_tree (fun _ l r -> 1 + max l r) 0 l

(* 8 *)
let l = [1; 2; 3; 2; 1; 2; 2; 56; 32; 2; 34; 4; 2]

let t = Unix.gettimeofday ()

let _ =
  for x = 1 to 10_000_000 do ignore (member 56 l) done

let t' = Unix.gettimeofday ()

let _ =
  for x = 1 to 10_000_000 do ignore (List.mem 56 l) done

let t'' = Unix.gettimeofday ()

let _ =
  Printf.printf "Our member took %f seconds\n" (t' -. t);
  Printf.printf "List.mem took %f seconds\n" (t'' -. t')

