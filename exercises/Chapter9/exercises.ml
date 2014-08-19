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
        if prelen > bestlen then
          longest_prefix_inner (currpos + 1) currpos prelen p t
        else
          longest_prefix_inner (currpos + 1) bestpos bestlen p t

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

let test () =
  Printf.printf "Naive version took %f seconds\n" (profile search_naive);
  Printf.printf "Better version tool %f seconds\n" (profile search_better)

(* 4. (See examples for this chapter) *)

(* 5. Case-insensitive search *)
let search ?(nocase = false) ss s =
  if nocase then
    search' 0 (String.uppercase ss) (String.uppercase s)
  else
    search' 0 ss s

