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

let rec at ss ssp s sp =
  (*Printf.printf "\nssp = %i, sp = %i" ssp sp;*)
  ssp > String.length ss - 1 ||
    match
      match ss.[ssp] with
         '?' ->
           if ssp + 1 > String.length ss - 1 then None (* ? at end of pattern *)
           else if sp > String.length s - 1 then Some (String.length ss - ssp, 0) (* No char left to match *)
           else if ss.[ssp + 1] = s.[sp] then Some (2, 1) (* match *)
           else Some (2, 0) (* no match *)
       | '*' -> 
           if ssp + 1 > String.length ss - 1 then None else (* * at end of pattern *)
             Some (2, swallow_all ss.[ssp + 1] ss sp) (* read zero or more items *)
       | '+' ->
           if ssp + 1 > String.length ss - 1 then None (* + at end of pattern *)
           else if sp > String.length s - 1 then None (* Nothing left to match *)
           else if ss.[ssp + 1] = s.[sp] then
             Some (2, swallow_all ss.[ssp + 1] ss sp) (* read one or more items *)
           else None (* did not match *)
       | '\\' -> (* Next character to be taken literally *)
           let matched =
             (sp < String.length s &&
              ssp < String.length ss - 1 &&
              ss.[ssp + 1] = s.[sp])
           in
             if matched then Some (2, 1) else None
       | c ->
           if sp < String.length s && c = s.[sp] then None else Some (1, 1)
    with
      None -> false
    | Some (jump_ss, jump_s) -> at ss (ssp + jump_ss) s (sp + jump_s)

let rec search' n ss s =
  n <= String.length s && (at ss 0 s n || search' (n + 1) ss s)

let search = search' 0

let tests = 
  [("", "");
   ("a", "");
   ("a", "a");
   ("ab", "aaab");
   ("ab", "b");
   ("ab", "c");
   ("+", "");
   ("+a", "");
   ("+a", "a");
   ("+ab", "aaab");
   ("+ab", "b");
   ("+ab", "c");
   ("*", "");
   ("*a", "");
   ("*a", "a");
   ("*ab", "aaab");
   ("*ab", "b");
   ("*ab", "c");
   ("?", "");
   ("?a", "");
   ("?a", "a");
   ("?ab", "aaab");
   ("?ab", "b");
   ("?ab", "c")]

let _ =
  List.iter
    (fun (pattern, str) ->
       Printf.printf "Pattern %-10sString %-10s%-10b\n"
       pattern str (search pattern str))
    tests

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

