let substring = "row"

let string = "this is a rower."

    let take l n =
      if n < 0 then raise (Invalid_argument "take") else
      let rec take_inner r l n =
        if n = 0 then List.rev r else
          match l with
          | [] -> raise (Invalid_argument "take")
          | h::t -> take_inner (h::r) t (n - 1)
      in
        take_inner [] l n

    let rec drop_inner n = function
      | [] -> raise (Invalid_argument "drop")
      | _::t -> if n = 1 then t else drop_inner (n - 1) t

    let drop l n =
      if n < 0 then raise (Invalid_argument "drop") else
      if n = 0 then l else
        drop_inner n l


let explode s =
  let l = ref [] in
    for p = String.length s downto 1 do
      l := s.[p - 1] :: !l
    done;
    !l

let implode l =
  let s = String.create (List.length l) in
    let rec list_loop x = function
       [] -> ()
     | i::t -> s.[x] <- i; list_loop (x + 1) t
    in
      list_loop 0 l;
      s

(* Search for a sublist in a list. *)
let rec search_list p l =
    List.length p <= List.length l
  && 
    (take l (List.length p) = p || search_list p (List.tl l))

(* Without all the List.lengths *)
let rec search_list_inner p len_p l len_l =
    len_p <= len_l
  &&
    (take l len_p = p ||
     search_list_inner p len_p (List.tl l) (len_l - 1))

let search_list p l =
  search_list_inner p (List.length p) l (List.length l)

(* Removing Util.take *)
let rec equal l len p =
     len = 0
  ||
     List.hd l = List.hd p && equal (List.tl l) (len - 1) (List.tl p)

let rec search_list_inner p len_p l len_l =
    len_p <= len_l
  &&
    (equal l len_p p ||
     search_list_inner p len_p (List.tl l) (len_l - 1))

let search_list p l =
  search_list_inner p (List.length p) l (List.length l)

(* Wrap it up *)
let search ss s =
  search_list (explode ss) (explode s)

(* The same algorithm, directly translated into basic strings. *)
let rec search ss s =
    String.length ss <= String.length s
  &&
    (String.sub s 0 (String.length ss) = ss ||
     search ss (String.sub s 1 (String.length s - 1)))

(* Now, do it in place. *)
let rec at ss ssp s sp l =
  l = 0 || ss.[ssp] = s.[sp] && at ss (ssp + 1) s (sp + 1) (l - 1)

let rec search' n ss s =
    String.length ss <= String.length s - n
  &&
    (at ss 0 s n (String.length ss) || search' (n + 1) ss s)

let search = search' 0

(* Boyer Moore *)


(* Exercises *)

(* 1a. Number of matches of a pattern in a string. All matches considered *)
let rec at ss ssp s sp l =
  l = 0 || ss.[ssp] = s.[sp] && at ss (ssp + 1) s (sp + 1) (l - 1)

let rec search' matches n ss s =
  if String.length ss > String.length s - n then matches else
    if at ss 0 s n (String.length ss)
      then search' (matches + 1) (n + 1) ss s
      else search' matches (n + 1) ss s

let search = search' 0 0

(* 1b. Same, only non-overlapping matches considered. *)
let rec search' matches n ss s =
  if String.length ss > String.length s - n then matches else
    if at ss 0 s n (String.length ss)
      then search' (matches + 1) (n + String.length ss) ss s
      else search' matches (n + 1) ss s

let search = search' 0 0

(* 2. Longest prefix for lists *)

(* Return the length of the longest prefix of p at the beginning of l *)
let rec prefix p l =
  match p, l with
    ph::pt, lh::lt -> if ph = lh then 1 + prefix pt lt else 0
  | _ -> 0

let rec longest_prefix_inner currpos bestpos bestlen p l =
  match l with
    [] -> (bestpos, bestlen)
  | h::t ->
      let prelen = prefix p l in
        if prelen > bestlen
          then longest_prefix_inner (currpos + 1) currpos prelen p t
          else longest_prefix_inner (currpos + 1) bestpos bestlen p t

let longest_prefix p l =
  longest_prefix_inner 0 0 0 p l

(* 3. Profile string_search functions x 3 *)
let rec search_naive ss s =
    String.length ss <= String.length s
  &&
    (String.sub s 0 (String.length ss) = ss ||
     search_naive ss (String.sub s 1 (String.length s - 1)))

(* Now, do it in place. *)
let rec at ss ssp s sp l =
  l = 0 || ss.[ssp] = s.[sp] && at ss (ssp + 1) s (sp + 1) (l - 1)

let rec search' n ss s =
    String.length ss <= String.length s - n
  &&
    (at ss 0 s n (String.length ss) || search' (n + 1) ss s)

let search_better = search' 0


let profile f =
  let t = Unix.gettimeofday () in
    for x = 1 to 1_000_000 do
      ignore (f "ABA" "Somewhere in here is the pattern ABBBBAABA.")
    done;
    Unix.gettimeofday () -. t

let _ =
  Printf.printf "Naive version took %f seconds\n" (profile search_naive);
  Printf.printf "Better version tool %f seconds\n" (profile search_better)

