(* Introduction - factorials, lists, sets *)

(* A. Basic method *)
let rec interleave h l ls =
  match ls with
    [] -> [l @ [h]]
  | x::xs -> (l @ (h :: x :: xs)) :: interleave h (l @ [x]) xs

let combine x ps =
  List.concat (List.map (interleave x []) ps)

let rec perms p =
  match p with
    [] -> [[]]
  | h::t -> combine h (perms t)

(* # List.length (perms [1;2;3;4;5;6;7;8]);;
- : int = 40320
# List.length (perms [1;2;3;4;5;6;7;8;9]);;
- : int = 362880
# List.length (perms [1;2;3;4;5;6;7;8;9;10]);;
Stack overflow during evaluation (looping recursion?). *)

(* B. Make this tail recursive -- now 10 & 11 work, 12 too big to calculate. *)
let rec interleave acc h l ls =
  match ls with
    [] -> (l @ [h]) :: acc
  | x::xs -> interleave ((l @ (h :: x :: xs)) :: acc) h (l @ [x]) xs

let combine x ps =
  List.concat (List.map (interleave [] x []) ps)

let rec perms p =
  match p with
    [] -> [[]]
  | h::t -> combine h (perms t)

(* C. Another method. pick each element out, and use it as the first element. Easy with sets: *) 
let rec without x l =
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
          l)

(* E. Version which can give the next permutation -- lexicographic permutation *)
let firstchar arr =
  let f = ref (Array.length arr - 1) in
    for x = 0 to Array.length arr - 2 do
      if arr.(x) < arr.(x + 1) then f := x
    done;
    !f

let ceiling arr f =
  let c = ref (-1) in
    for x = Array.length arr - 1 downto f + 1 do
      if arr.(x) > arr.(f) && (!c = (-1) || arr.(x) < arr.(!c)) then c := x
    done;
    !c

let swap arr a b =
  let t = arr.(a) in
    arr.(a) <- arr.(b);
    arr.(b) <- t

let array_rev a o l =
  for x = 0 to l / 2 - 1 do
    swap a (o + x) (o + l - x - 1)
  done

let sort_subarray arr o l =
  let sub = Array.sub arr o l in
    Array.sort compare sub;
    Array.blit sub 0 arr o l

let next_permutation arr_in =
  let arr = Array.copy arr_in in
  let f = firstchar arr in
  let c = ceiling arr f in
    swap arr f c;
    sort_subarray arr (f + 1) (Array.length arr - 1 - f);
    arr

let non_increasing arr =
  Array.length arr <= 1 ||
    let r = ref true in
      for x = 0 to Array.length arr - 2 do
        if arr.(x + 1) > arr.(x) then r := false
      done;
      !r

let all_permutations arr =
  let copy = Array.copy arr in
    Array.sort compare copy;
    let perm = ref copy in
    let perms = ref [copy] in        
      while not (non_increasing !perm) do
        perm := next_permutation !perm;
        perms := !perm :: !perms;
      done;
      Array.of_list (List.rev !perms)

(* D. Lazy method from next perm one. *)
type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec perms x =
  Cons (x, fun () ->
    if non_increasing x then
      let c = Array.copy x in
        Array.sort compare c;
        perms c
    else
      perms (next_permutation x))
  
