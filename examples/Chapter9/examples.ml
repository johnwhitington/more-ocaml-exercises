open More

let substring = "row"

let string = "this is a rower."

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
    (Util.take l (List.length p) = p || search_list p (List.tl l))

(* Without all the List.lengths *)
let rec search_list_inner p len_p l len_l =
    len_p <= len_l
  &&
    (Util.take l len_p = p ||
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

(* The same algorith, but with:

? : zero or one matches of the next character
* : zero or more matches of the next character
+ : one or more matches of the next character
\ : escape *)

(* Returns the number of characters, zero or more, equal to 'ch' in ss starting
 * at sp *)
let swallow_all ch ss sp =
  let p = ref sp in
    while !p < String.length ss && ss.[!p] = ch do p := !p + 1 done;
    !p

let rec at ss ssp s sp l =
  (*Printf.printf "\nssp = %i, sp = %i, l = %i\n" ssp sp l;*)
  l = 0 ||
    let matched, jump_ss, jump_s, jump_l =
      match ss.[ssp] with
         '?' ->
           if ssp + 1 > String.length ss - 1 then (false, 0, 0, l) (* ? at end of pattern *)
           else if sp > String.length s - 1 then (true, 0, 0, l) (* No char left to match *)
           else if ss.[ssp + 1] = s.[sp] then (true, 2, 1, 2) (* match *)
           else (true, 2, 0, 2) (* no match *)
       | '*' -> 
           if ssp + 1 > String.length ss - 1 then (false, 0, 0, l) else (* * at end of pattern *)
             let n = swallow_all ss.[ssp + 1] ss sp in (true, 2, n, 2) (* read zero or more items *)
       | '+' ->
           if ssp + 1 > String.length ss - 1 then (false, 0, 0, l) (* + at end of pattern *)
           else if sp > String.length s - 1 then (false, 0, 0, l) (* Nothing left to match *)
           else if ss.[ssp + 1] = s.[sp] then
             let n = swallow_all ss.[ssp + 1] ss sp in (true, 2, n, 2) (* read one or more items *)
           else (false, 0, 0, l) (* did not match *)
       | '\\' ->
           (* Next character to be taken literally *)
           let matched =
             (sp < String.length s &&
              ssp < String.length ss - 1 &&
              ss.[ssp + 1] = s.[sp])
           in
             (matched, 2, 1, 2)
       | c -> (sp < String.length s && c = s.[sp], 1, 1, 1)
    in
      matched && at ss (ssp + jump_ss) s (sp + jump_s) (l - jump_l)

let rec search' n ss s =
  Printf.printf "\nsearch' %i\n" n;
  n <= String.length s &&
  (at ss 0 s n (String.length ss) || search' (n + 1) ss s)

let search = search' 0

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

(* 5. Case-insensitive search *)
let search ?(nocase = false) ss s =
  if nocase then
    search' 0 (String.uppercase ss) (String.uppercase s)
  else
    search' 0 ss s

