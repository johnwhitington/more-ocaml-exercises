open More

(* Search for a pattern in a list. *)
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

(* The same algorithm, directly translated into basic strings. *)
let rec search p s =
    String.length p <= String.length s
  &&
    (String.sub s 0 (String.length p) = p ||
     search p (String.sub s 1 (String.length s - 1)))

(* Now, do it in place. *)
let rec at p pp s sp l =
  l = 0 || p.[pp] = s.[sp] && at p (pp + 1) s (sp + 1) (l - 1)

let rec search' n p s =
    String.length p <= String.length s - n
  &&
    (at p 0 s n (String.length p) || search' (n + 1) p s)

let search = search' 0

(* The same algorith, but with:

? : zero or one matches of the next character
* : zero or more matches of the next character
+ : one or more matches of the next character
\ : escape *)

(* Returns the number of characters, zero or more, equal to 'ch' in ss starting
 * at sp *)
let swallow_all ch p sp =
  let x = ref sp in
    while !x < String.length p && p.[!x] = ch do x := !x + 1 done;
    !x

let rec at p pp s sp =
  pp > String.length p - 1 || (* used whole pattern *)
    match
      match p.[pp] with
       | '?' ->
           if pp + 1 > String.length p - 1 then None (* ? at end of pattern *)
           else if sp > String.length s - 1 then Some (2, 0) (* No char left to match *)
           else if p.[pp + 1] = s.[sp] then Some (2, 1) (* match *)
           else Some (2, 0) (* no match *)
       | '*' -> 
           if pp + 1 > String.length p - 1 then None else (* * at end of pattern *)
             Some (2, swallow_all p.[pp + 1] p sp) (* read zero or more items *)
       | '+' ->
           if pp + 1 > String.length p - 1 then None (* + at end of pattern *)
           else if sp > String.length s - 1 then None (* Nothing left to match *)
           else if p.[pp + 1] = s.[sp] then
             Some (2, swallow_all p.[pp + 1] p sp) (* read one or more items *)
           else None (* did not match *)
       | '\\' -> (* Next character to be taken literally *)
           let matched =
             (sp < String.length s &&
              pp < String.length p - 1 &&
              p.[pp + 1] = s.[sp])
           in
             if matched then Some (2, 1) else None
       | c ->
           if sp < String.length s && c = s.[sp] then Some (1, 1) else None
    with
      None -> false
    | Some (jump_p, jump_s) -> at p (pp + jump_p) s (sp + jump_s)

let rec search' n p s =
  (n < String.length s || n = 0 && String.length s = 0) &&
  (at p 0 s n || search' (n + 1) p s)

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

let test () =
  List.iter
    (fun (pattern, str, expected) ->
       let result = search pattern str in
       Printf.printf "Pattern %-10sString %-10s Expected %-14b   Actual %-14b   Correct %-14b\n"
       pattern str expected result (expected = result))
    tests

