(* Tail-recursive lists *)
module List :
  sig
    include (module type of List)
  end
=
  struct
    include List

    let map f l =
      List.rev (List.rev_map f l)

    let append a b =
      List.rev_append (List.rev a) b

    let ( @ ) = append

    let map f l =
      List.rev (List.rev_map f l)

    let rev_map2 f l l2 =
      let rec rev_map_inner acc a b =
        match a, b with
          [], [] -> acc
        | x::xs, y::ys -> rev_map_inner (f x y :: acc) xs ys
        | _ -> raise (Invalid_argument "List.map2")
      in
        rev_map_inner [] l l2

    let map2 f l l2 =
      List.rev (rev_map2 f l l2)

    let concat lists =
      let rec concat out acc =
        match acc with
        | [] -> out
        | l::ls -> concat (append l out) ls
      in
        concat [] (List.rev lists)

    let fold_right f l e =
      List.fold_left (fun x y -> f y x) e (List.rev l)
  end

(* Utilities *)
module Util :
  sig
    val from : int -> int -> int list
    val take : 'a list -> int -> 'a list
    val drop : 'a list -> int -> 'a list
  end
=
  struct
    let from s e =
      if e < s then raise (Invalid_argument "from") else
        let n = ref [] in
          for x = s to e do n := x :: !n done;
          List.rev !n

    let take l n =
      if n < 0 then raise (Invalid_argument "take") else
      let rec take_inner r l n =
        if n = 0 then List.rev r else
          match l with
            [] -> raise (Invalid_argument "take")
          | h::t -> take_inner (h :: r) t (n - 1)
      in
        take_inner [] l n

    let drop l n =
      let rec drop_inner n l =
        match l with
          [] -> raise (Invalid_argument "drop")
        | _::t -> if n = 1 then t else drop_inner (n - 1) t
      in
        if n < 0 then raise (Invalid_argument "drop") else
        if n = 0 then l else
          drop_inner n l
  end

