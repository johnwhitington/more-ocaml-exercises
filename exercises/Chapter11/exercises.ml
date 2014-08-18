(* Set representations *)
open More

(* The answer to Question 1 is found in the examples. *)

(* Questions 2 & 4 combined *)
module type SetType =
  sig
    type 'a t
    val set_of_list : 'a list -> 'a t
    val list_of_set : 'a t -> 'a list
    val insert : 'a -> 'a t -> 'a t
    val size : 'a t -> int
    val member : 'a -> 'a t -> bool
    val union : 'a t -> 'a t -> 'a t
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

    let union a b = List.fold_left (fun x y -> insert y x) a b
  end

module SetTree : sig include SetType end =
  struct
    type 'a t =
      Lf
    | Br of 'a t * 'a * 'a t

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
      | Br (l, y, r) ->
          if x < y then member x l else member x r

    let union a b =
      List.fold_left (fun x y -> insert y x) a (list_of_set b)
  end

(* And with different Br nodes *)
module SetRedBlack : sig include SetType end =
  struct
    type 'a t = 
      Lf
    | BrR of 'a t * 'a * 'a t
    | BrB of 'a t * 'a * 'a t

    let rec list_of_set s =
      match s with
        Lf -> []
      | BrR (l, x, r) | BrB (l, x, r) ->
          x :: list_of_set l @ list_of_set r
    
    let balance n =
      match n with
        BrB (BrR (BrR (a, x, b), y, c), z, d)
      | BrB (BrR (a, x, BrR (b, y, c)), z, d)
      | BrB (a, x, BrR (BrR (b, y, c), z, d))
      | BrB (a, x, BrR (b, y, BrR (c, z, d))) ->
          BrR (BrB (a, x, b), y, BrB (c, z, d))
      | BrR (a, b, c) -> BrR (a, b, c)
      | BrB (a, b, c) -> BrB (a, b, c)
      | Lf -> Lf

    let rec add_inner x s =
      match s with
        Lf -> BrR (Lf, x, Lf)
      | BrR (l, y, r) ->
          if x < y
            then balance (BrR (add_inner x l, y, r))
            else if x > y then balance (BrR (l, y, add_inner x r))
            else BrR (l, y, r)
      | BrB (l, y, r) ->
          if x < y
            then balance (BrB (add_inner x l, y, r))
            else if x > y then balance (BrB (l, y, add_inner x r))
            else BrB (l, y, r)

    let insert x s =
      match add_inner x s with
        BrR (l, y, r) | BrB (l, y, r) -> BrB (l, y, r)
      | Lf -> assert false

    let rec set_of_list l =
      match l with
        [] -> Lf
      | h::t -> insert h (set_of_list t)

    let rec size s =
      match s with
        Lf -> 0
      | BrR (l, _, r) | BrB (l, _, r) -> 1 + size l + size r

    let rec member x s =
      match s with
        Lf -> false
      | BrR (l, y, r) | BrB (l, y, r) ->
          x = y || if x > y then member x r else member x l

    let union a b =
      List.fold_left (fun x y -> insert y x) a (list_of_set b)
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

    let union a b =
      set_of_list (list_of_set a @ list_of_set b)
  end


(* Question 3. *)
module IntSet :
  sig
    type t
    val set_of_list : int list -> t
    val list_of_set : t -> int list
    val insert : int -> t -> t
    val size : t -> int
    val member : int -> t -> bool
  end
=
  struct
    module S = Set.Make (struct type t = int let compare = compare end)
    
    type t = S.t 

    let list_of_set s = S.elements s

    let set_of_list l = List.fold_right S.add l S.empty

    let member = S.mem

    let insert = S.add

    let size = S.cardinal
  end

let nums = Util.from 1 50000

let rand = Array.to_list (Array.init 50000 (fun _ -> Random.int 1073741823))

(* Benchmark sets from sets. *)
let benchmark_intset name ns =
  let a = Unix.gettimeofday () in
    let set = IntSet.set_of_list ns in
      let b = Unix.gettimeofday () in
        List.iter (fun x -> ignore (IntSet.member x set)) ns;
        let c = Unix.gettimeofday () in
          Printf.printf
            "For %s, insertion took %f, membership %f\n"
            name (b -. a) (c -. b)

let _ =
  benchmark_intset "ordered" nums;
  benchmark_intset "unordered" rand

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



