open More
open Input
open Bits

let white_terminating_codes =
  [|[0; 0; 1; 1; 0; 1; 0; 1]; 
    [0; 0; 0; 1; 1; 1];
    [0; 1; 1; 1];
    [1; 0; 0; 0];
    [1; 0; 1; 1];
    [1; 1; 0; 0];
    [1; 1; 1; 0];
    [1; 1; 1; 1];
    [1; 0; 0; 1; 1];
    [1; 0; 1; 0; 0];
    [0; 0; 1; 1; 1];
    [0; 1; 0; 0; 0];
    [0; 0; 1; 0; 0; 0];
    [0; 0; 0; 0; 1; 1];
    [1; 1; 0; 1; 0; 0];
    [1; 1; 0; 1; 0; 1];
    [1; 0; 1; 0; 1; 0];
    [1; 0; 1; 0; 1; 1];
    [0; 1; 0; 0; 1; 1; 1];
    [0; 0; 0; 1; 1; 0; 0];
    [0; 0; 0; 1; 0; 0; 0];
    [0; 0; 1; 0; 1; 1; 1];
    [0; 0; 0; 0; 0; 1; 1];
    [0; 0; 0; 0; 1; 0; 0];
    [0; 1; 0; 1; 0; 0; 0];
    [0; 1; 0; 1; 0; 1; 1];
    [0; 0; 1; 0; 0; 1; 1];
    [0; 1; 0; 0; 1; 0; 0];
    [0; 0; 1; 1; 0; 0; 0];
    [0; 0; 0; 0; 0; 0; 1; 0];
    [0; 0; 0; 0; 0; 0; 1; 1];
    [0; 0; 0; 1; 1; 0; 1; 0];
    [0; 0; 0; 1; 1; 0; 1; 1];
    [0; 0; 0; 1; 0; 0; 1; 0];
    [0; 0; 0; 1; 0; 0; 1; 1];
    [0; 0; 0; 1; 0; 1; 0; 0];
    [0; 0; 0; 1; 0; 1; 0; 1];
    [0; 0; 0; 1; 0; 1; 1; 0];
    [0; 0; 0; 1; 0; 1; 1; 1];
    [0; 0; 1; 0; 1; 0; 0; 0];
    [0; 0; 1; 0; 1; 0; 0; 1];
    [0; 0; 1; 0; 1; 0; 1; 0];
    [0; 0; 1; 0; 1; 0; 1; 1];
    [0; 0; 1; 0; 1; 1; 0; 0];
    [0; 0; 1; 0; 1; 1; 0; 1];
    [0; 0; 0; 0; 0; 1; 0; 0];
    [0; 0; 0; 0; 0; 1; 0; 1];
    [0; 0; 0; 0; 1; 0; 1; 0];
    [0; 0; 0; 0; 1; 0; 1; 1];
    [0; 1; 0; 1; 0; 0; 1; 0];
    [0; 1; 0; 1; 0; 0; 1; 1];
    [0; 1; 0; 1; 0; 1; 0; 0];
    [0; 1; 0; 1; 0; 1; 0; 1];
    [0; 0; 1; 0; 0; 1; 0; 0];
    [0; 0; 1; 0; 0; 1; 0; 1];
    [0; 1; 0; 1; 1; 0; 0; 0];
    [0; 1; 0; 1; 1; 0; 0; 1];
    [0; 1; 0; 1; 1; 0; 1; 0];
    [0; 1; 0; 1; 1; 0; 1; 1];
    [0; 1; 0; 0; 1; 0; 1; 0];
    [0; 1; 0; 0; 1; 0; 1; 1];
    [0; 0; 1; 1; 0; 0; 1; 0];
    [0; 0; 1; 1; 0; 0; 1; 1];
    [0; 0; 1; 1; 0; 1; 0; 0]|]

(* QUESTIONS *)

(* 1. Byte-by-byte on lists. *)

(* Compression, not tail-recursive *)

(* Type for runs *)
type run = Same of int * int | Diff of int list

(* Return list of same characters from a non-null list, and rest *)
let rec get_same x n l =
  match l with
  | h::t when h = x -> get_same x (n + 1) t
  | _ -> (n, l)

(* Return list of different characters from a non-null list, until two are the
 * same as one another *)
let rec get_different a l =
  match l with
    [] -> (List.rev a, [])
  | h::t ->
      if a = [] then get_different [h] t
      else if h <> List.hd a then get_different (h :: a) t
      else (List.rev (List.tl a), List.hd a :: l)

(* Get a single run *)
let getrun l =
  match l with
    [] -> raise (Invalid_argument "getrun")
  | h::_ ->
      match get_same h 0 l with
        1, _ -> let diff, rest = get_different [] l in (Diff diff, rest)
      | n, rest -> (Same (n, h), rest)

(* Build the next chars from a run *)
let chars_of_run r =
  match r with
    Same (length, c) -> [257 - length; c]
  | Diff chars -> List.length chars - 1 :: chars 

(* Compression, tail-recursive *)
let rec compress_inner a l =
  match l with
    [] -> List.concat (List.map chars_of_run (List.rev a))
  | _ ->
      let run, rest = getrun l in
        compress_inner (run :: a) rest

let compress l = compress_inner [] l @ [128]

(* Decompression, tail-recursive *)
let rec decompress_inner a l =
  match l with
    [128] -> List.concat (List.rev a)
  | [] | [_] -> raise (Invalid_argument "decompress_inner")
  | h::t::t' ->
      if h < 127 then
        let bytes = Util.take (t :: t') (h + 1) in
        let rest = Util.drop (t :: t') (h + 1) in
          decompress_inner (bytes :: a) rest
      else if h > 128 then
        decompress_inner (Array.to_list (Array.make (257 - h) t) :: a) t' 
      else decompress_inner a []

let decompress l = decompress_inner [] l

(* 2. Huffman tree from codes. *)
let from s e =
  if e < s then raise (Invalid_argument "from") else
    let n = ref [] in
      for x = s to e do n := x :: !n done;
      List.rev !n

type tree = Lf | Code of int | Br of tree * tree

let rec add_elt tr (l, n) =
  match l with
    0::m ->
      begin match tr with
        Lf -> Br (add_elt Lf (m, n), Lf)
      | Br (left, right) -> Br (add_elt left (m, n), right)
      | Code x -> raise (Failure "collision")
      end
  | 1::m ->
      begin match tr with
        Lf -> Br (Lf, add_elt Lf (m, n))
      | Br (left, right) -> Br (left, add_elt right (m, n))
      | Code x -> raise (Failure "collision")
      end
  | [] -> Code n
  | _ -> raise (Failure "bad code")

let make_tree arr numbers =
  List.fold_left
    add_elt
    Lf
    (List.combine (Array.to_list arr) numbers)

let white_terminating_tree =
  make_tree
    white_terminating_codes
    (Util.from 0 (Array.length white_terminating_codes - 1))

(* 4. Making a histogram of black and white frequencies. This is a pair of int
 * arrays of length 1792 for run lengths. *)
let getbitint b =
  if getbit b then 1 else 0
  
let peekbit b =
  if b.bit = 0 then
    begin
      let byte = int_of_char (b.input.input_char ()) in
        rewind b.input;
        byte land 128 > 0
    end
  else
    b.byte land b.bit > 0

let rec read_up_to v i n w =
  if n >= w then (n, v) else
    match peekbit i with
      x when x = v -> (ignore (getbit i)); read_up_to v i (n + 1) w
    | x -> (n, v)

let build_histogram a_white a_black i w h =
  let toread = ref (w * h) in
  let wleft = ref w in
    while !toread > 0 do
      let n, v = read_up_to (peekbit i) i 0 !wleft in
        let a = if v then a_black else a_white in
          a.(n) <- a.(n) + 1;
          toread := !toread - n;
          wleft := !wleft - n;
          if !wleft = 0 then wleft := w
    done

let histogram_of_input i w h =
  let white = Array.make 1792 0 in
  let black = Array.make 1792 0 in
    build_histogram white black (input_bits_of_input i) w h;
    (white, black)

let print_histogram =
  Array.iteri
    (fun x n -> if n > 0 then Printf.printf "%i runs of length %i\n" n x)

(* FIXME: Add code to actually print out the example when this is run. *)

