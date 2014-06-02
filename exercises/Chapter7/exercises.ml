(* 1 *)
let make ~len ~elt =
  Array.make len elt

(* 2 *)
type start = Start of int

type length = Length of int

let fill a (Start s) (Length l) v =
  for x = s to s + l - 1 do a.(x) <- v done

let filled () =
  let a = Array.make 100 "x" in
    fill a (Start 20) (Length 40) "y";
    a

(* 3 *)

(* 4 *)
let sub b ~off ~len =
  Buffer.sub b off len

let blit src ~srcoff dst ~dstoff ~len =
  Buffer.blit src srcoff dst dstoff len

let add_substring b s ~ofs ~len =
  Buffer.add_substring b s ofs len

(* 5 *)
let rec map_inner a f l =
  match l with
    [] -> List.rev a
  | h::t -> map_inner (f h :: a) f t

let map f l = map_inner [] f l

let rec map ?(a = []) f l =
  match l with
    [] -> List.rev a
  | h::t -> map ~a:(f h :: a) f t

