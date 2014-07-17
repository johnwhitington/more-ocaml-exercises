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
  ssp > String.length ss - 1 || (* used whole pattern *)
    match
      match ss.[ssp] with
       | '?' ->
           if ssp + 1 > String.length ss - 1 then None (* ? at end of pattern *)
           else if sp > String.length s - 1 then Some (2, 0) (* No char left to match *)
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
           if sp < String.length s && c = s.[sp] then Some (1, 1) else None
    with
      None -> false
    | Some (jump_ss, jump_s) -> at ss (ssp + jump_ss) s (sp + jump_s)

let rec search' n ss s =
  (n < String.length s || n = 0 && String.length s = 0) &&
  (at ss 0 s n || search' (n + 1) ss s)

let search = search' 0

let tests = 
  [("", "", true);
   ("a", "", false);
   ("a", "a", true);
   ("ab", "aaab", true);
   ("ab", "b", false);
   ("ab", "c", false);
   ("+", "", false); (* bad pattern *)
   ("+a", "", false);
   ("+a", "a", true);
   ("+ab", "aaab", true);
   ("+ab", "b", false);
   ("+ab", "c", false);
   ("*", "", false); (* bad pattern *)
   ("*a", "", true);
   ("*a", "a", true);
   ("*ab", "aaab", true);
   ("*ab", "b", true);
   ("*ab", "c", false);
   ("?", "", false); (* bad pattern *)
   ("?a", "", true);
   ("?ab", "", false);
   ("?a", "a", true);
   ("?ab", "aaab", true);
   ("?ab", "b", true);
   ("?ab", "c", false);
   ("\\a", "a", true);
   ("\\\\", "\\", true);
   ("\\?", "?", true);
   ("\\+", "+", true);
   ("\\*", "*", true)]

let _ =
  List.iter
    (fun (pattern, str, expected) ->
       let result = search pattern str in
       Printf.printf "Pattern %-10sString %-10s Expected %-14b   Actual %-14b   Correct %-14b\n"
       pattern str expected result (expected = result))
    tests

