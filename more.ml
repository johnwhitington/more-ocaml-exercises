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

    (* FIXME. Add map2, and make both tail-recursive *)
    let rec map3 f l l2 l3 =
      match l, l2, l3 with
       [], [], [] -> []
      | x::xs, y::ys, z::zs -> f x y z :: map3 f xs ys zs
      | _ -> raise (Invalid_argument "map3")

    let concat lists =
      let rec concat out = function
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
  end

