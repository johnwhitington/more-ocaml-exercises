(* Tail-recursive lists *)
module List :
  sig
    include (module type of List)
  end

(* Utilities *)
module Util :
  sig
    val from : int -> int -> int list
    val take : 'a list -> int -> 'a list
    val drop : 'a list -> int -> 'a list
  end

