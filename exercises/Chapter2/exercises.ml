type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

(* 1 *)
let rec ldouble n =
  Cons (n, fun () -> ldouble (n * 2))

let thedoubles = ldouble 1

(* 2 *)
let rec lnth (Cons (h, tf)) n =
  match n with
    0 -> h
  | _ -> lnth (tf ()) (n - 1)

(* 3 *)
let rec lrepeating_inner c l =
  match c with
    [] -> raise (Invalid_argument "lrepeating: empty list")
  | [x] -> Cons (x, fun () -> lrepeating_inner l l)
  | h::t -> Cons (h, fun () -> lrepeating_inner t l)

let lrepeating l = lrepeating_inner l l

(* 4 *)
let rec fibonacci_inner x y =
  Cons (x, fun () -> fibonacci_inner y (x + y))

let fibonacci = fibonacci_inner 0 1

(* 5 *)
let rec unleave (Cons (h, tf)) =
  let Cons (h', tf') = tf () in
    let t = tf' () in
      (Cons (h, fun () -> fst (unleave t)),
       Cons (h', fun () -> snd (unleave t)))

(* 6 *)
let rec letter_string n =
  if n <= 26 then
    Char.escaped (char_of_int (n + 64))
  else
    letter_string ((n - 1) / 26) ^
    letter_string (((n - 1) mod 26) + 1)

let rec lseq n =
  Cons (n, fun () -> lseq (n + 1))

let rec lmap f (Cons (h, tf)) =
  Cons (f h, fun () -> lmap f (tf ()))

let alphas =
  lmap letter_string (lseq 1)
