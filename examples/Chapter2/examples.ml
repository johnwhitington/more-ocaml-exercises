type 'a lazylist = Cons of 'a * (unit -> 'a lazylist)

let rec lseq n =
  Cons (n, fun () -> lseq (n + 1))

let lhd (Cons (n, _)) = n

let ltl (Cons (_, tf)) = tf ()

let rec ltake (Cons (h, tf)) n =
  match n with
    0 -> []
  | _ -> h :: ltake (tf ()) (n - 1)

let rec ldrop (Cons (h, tf) as ll) n =
  match n with
    0 -> ll
  | _ -> ldrop (tf ()) (n - 1)

let rec lmap f (Cons (h, tf)) =
  Cons (f h, fun () -> lmap f (tf ()))

let rec lfilter f (Cons (h, tf)) =
  if f h then
    Cons (h, fun () -> lfilter f (tf ()))
  else
    lfilter f (tf ())

let cubes =
  lfilter
    (fun x -> x mod 5 = 0)
    (lmap (fun x -> x * x * x) (lseq 1))

let rec mkprimes (Cons (h, tf)) =
  Cons (h, fun () ->
    mkprimes (lfilter (fun x -> x mod h <> 0) (tf ())))

let primes = mkprimes (lseq 2)

let rec interleave (Cons (h, tf)) l =
  Cons (h, fun () -> interleave l (tf ()))

let rec lconst n =
  Cons (n, fun () -> lconst n)

let interleaved =
  interleave (lconst 0) (lconst 1)

let rec allfrom l =
  Cons (l, fun () ->
    interleave (allfrom (0 :: l)) (allfrom (1 :: l)))

let allones = allfrom []

