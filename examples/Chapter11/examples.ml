(* Set representations *)
open More

module type SetType =
  sig
    type 'a t
    val set_of_list : 'a list -> 'a t
    val list_of_set : 'a t -> 'a list
    val insert : 'a -> 'a t -> 'a t
    val size : 'a t -> int
    val member : 'a -> 'a t -> bool
  end

module SetList : sig include SetType end =
  struct
    type 'a t = 'a list

    let list_of_set x = x

    let insert x l =
      if List.mem x l then l else x :: l

    let rec set_of_list l =
      match l with [] -> [] | h::t -> insert h (set_of_list t)

    let size = List.length

    let member = List.mem
  end

module SetTree : sig include SetType end =
  struct
    type 'a t = Lf | Br of 'a t * 'a * 'a t

    let rec list_of_set s =
      match s with
        Lf -> []
      | Br (l, x, r) -> list_of_set l @ [x] @ list_of_set r

    let rec insert x s =
      match s with
        Lf -> Br (Lf, x, Lf)
      | Br (l, y, r) ->
          if x = y then Br (l, y, r)
          else if x < y then Br (insert x l, y, r)
          else Br (l, y, insert x r)

    let rec set_of_list l =
      match l with
        [] -> Lf
      | h::t -> insert h (set_of_list t)

    let rec size s =
      match s with
        Lf -> 0
      | Br (l, _, r) -> 1 + size l + size r

    let rec member x s =
      match s with
        Lf -> false
      | Br (_, y, _) when x = y -> true
      | Br (l, y, r) -> if x < y then member x l else member x r
  end

module SetRedBlack : sig include SetType end =
  struct
    type colour = R | B

    type 'a t =
        Lf
      | Br of colour * 'a t * 'a * 'a t

    let rec list_of_set s =
      match s with
        Lf -> []
      | Br (_, l, x, r) -> x :: list_of_set l @ list_of_set r

    let balance t =
      match t with
        (B, Br (R, Br (R, a, x, b), y, c), z, d)
      | (B, Br (R, a, x, Br (R, b, y, c)), z, d)
      | (B, a, x, Br (R, Br (R, b, y, c), z, d))
      | (B, a, x, Br (R, b, y, Br (R, c, z, d))) ->
          Br (R, Br (B, a, x, b), y, Br (B, c, z, d))
      | (a, b, c, d) -> Br (a, b, c, d)

    let rec insert_inner x s =
      match s with
        Lf -> Br (R, Lf, x, Lf)
      | Br (c, l, y, r) ->
          if x < y
            then balance (c, insert_inner x l, y, r)
            else if x > y then balance (c, l, y, insert_inner x r)
            else Br (c, l, y, r)

    let insert x s =
      match insert_inner x s with
        Br (_, l, y, r) -> Br (B, l, y, r)
      | Lf -> assert false

    let rec set_of_list l =
      match l with
        [] -> Lf
      | h::t -> insert h (set_of_list t)

    let rec size s =
      match s with
        Lf -> 0
      | Br (_, l, _, r) -> 1 + size l + size r

    let rec member x s =
      match s with
        Lf -> false
      | Br (_, l, y, r) ->
          x = y || if x > y then member x r else member x l
  end


module SetHashtbl : sig include SetType end =
  struct
    type 'a t = ('a, unit) Hashtbl.t

    let list_of_set s =
      Hashtbl.fold (fun x () l -> x :: l) s []

    let set_of_list l =
      let s = Hashtbl.create (List.length l) in
        List.iter (fun x -> Hashtbl.add s x ()) l;
        s

    let member x s = Hashtbl.mem s x

    let insert x s =
      if not (member x s) then Hashtbl.add s x ();
      s

    let size = Hashtbl.length
  end


(* Numbers from 1 to 50000 *)
let nums = Util.from 1 50000

(* 50000 pseudorandom numbers between 0 and 1073741823 *)
let rand = Array.to_list (Array.init 50000 (fun _ -> Random.int 1073741823))


(* Build modules for each using first class modules syntax not discussed in the
 * text. *)
let implementations =
  [("Lists", (module SetList : SetType));
   ("Trees", (module SetTree : SetType));
   ("Red-black trees", (module SetRedBlack : SetType));
   ("Hash tables", (module SetHashtbl : SetType))]

(* Insert items into a set *)
let insertion_benchmark str l =
  List.iter
    (fun (n, s) ->
      let module S = (val s : SetType) in
        let t_start = Unix.gettimeofday () in
          let _ = S.set_of_list l in
            let t_end = Unix.gettimeofday () in
              Printf.printf
                "Insertion of 50000 %s elements with %s took %f seconds\n%!"
                str n (t_end -. t_start))
    implementations

(* Test membership of all items in a set, once populated with them *)
let membership_benchmark str l =
  List.iter
    (fun (n, s) ->
      let module S = (val s : SetType) in
        let set = S.set_of_list l in
          let t_start = Unix.gettimeofday () in
          List.iter (fun x -> ignore (S.member x set)) nums;
          let t_end = Unix.gettimeofday () in
            Printf.printf
              "Membership in a set made from %s items with %s took %f seconds\n%!"
              str n (t_end -. t_start))
    implementations

(* Return a list of all items in a set *)
let elements_benchmark str l =
  List.iter
    (fun (n, s) ->
      let module S = (val s : SetType) in
        let set = S.set_of_list l in
          let t_start = Unix.gettimeofday () in
          ignore (S.list_of_set set);
          let t_end = Unix.gettimeofday () in
            Printf.printf
              "Elements of a set made from %s items with %s took %f seconds\n%!"
              str n (t_end -. t_start))
    implementations

(* Find the size of a set *)
let size_benchmark str l =
  List.iter
    (fun (n, s) ->
      let module S = (val s : SetType) in
        let set = S.set_of_list l in
          let t_start = Unix.gettimeofday () in
          ignore (S.size set);
          let t_end = Unix.gettimeofday () in
            Printf.printf
              "Size of a set made from %s items with %s took %f seconds\n%!"
              str n (t_end -. t_start))
    implementations

(* Find how much memory is used by inserting 50000 random elements into a set
 * in each set representation *)
let memory_benchmark l =
  List.iter
    (fun (n, s) ->
      let module S = (val s : SetType) in
        let min, prom, maj = Gc.counters () in
          let _ = S.set_of_list l in
            let min2, prom2, maj2 = Gc.counters () in
              Printf.printf
                "Memory used creating set made from %s items is %f \n%!"
                n ((min2 +. maj2 -. prom2) -. (min +. maj -. prom)))
    implementations

(*let _ =
  insertion_benchmark "ordered" nums;
  print_newline ();
  insertion_benchmark "unordered" rand;
  print_newline ();
  membership_benchmark "ordered" nums;
  print_newline ();
  membership_benchmark "unordered" rand;
  print_newline ();
  elements_benchmark "ordered" nums;
  print_newline ();
  elements_benchmark "unordered" rand;
  print_newline ();
  size_benchmark "ordered" nums;
  print_newline ();
  size_benchmark "unordered" rand;
  print_newline ();
  memory_benchmark nums;
  memory_benchmark rand *)

