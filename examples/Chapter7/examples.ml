open More

(* Labelled and Optional Arguments examples *)

(* Document with labels. Punning. Label different from name. Missing out labels. Boolean example. *)
let fill a s l v =
  for x = s to s + l - 1 do a.(x) <- v done

let filled () =
  let a = Array.make 100 "x" in
    fill a 20 40 "y";
    a

let fill a ~start:s ~length:l v =
  for x = s to s + l - 1 do a.(x) <- v done

let filled () =
  let a = Array.make 100 "x" in
    fill a ~start:20 ~length:40 "y";
    a

let filled () =
  let a = Array.make 100 "x" in
    fill a "y" ~length:20 ~start:40;
    a

let filled () =
  let a = Array.make 100 "x" in
    let st = 20 in
      let ln = 40 in
        fill a ~start:st ~length:ln "y";
        a

let fill a ~start ~length v =
  for x = start to start + length - 1 do a.(x) <- v done

let filled () =
  let a = Array.make 100 "x" in
    let start = 20 in
      let length = 40 in
        fill a ~start ~length "y";
        a

(* Commute arguments for partial application. *)

let divide x y = x / y

let f = divide 10000 

let _ = [f 100; f 50; f 20]

let divide ~x ~y = x / y

let f = divide ~x:10000 

let _ = [f 100; f 50; f 20]

let f = divide ~y:10000 

let _ = [f 100000; f 10000; f 1000]

(* Optional arguments and their role in extending APIs. *)
let rec split l =
  match l with
    [] -> []
  | h::t -> [h] :: split t

let rec split ~chunksize l =
  try
    Util.take l chunksize :: split ~chunksize (Util.drop l chunksize)
  with
    _ -> match l with [] -> [] | _ -> [l]

let rec split ?(chunksize=1) l =
  try
    Util.take l chunksize :: split ~chunksize (Util.drop l chunksize)
  with
    _ -> match l with [] -> [] | _ -> [l]

let rec split ?chunksize l =
  let ch =
    match chunksize with None -> 1 | Some x -> x
  in
    try
      Util.take l ch :: split ~chunksize:ch (Util.drop l ch)
    with
      _ -> match l with [] -> [] | _ -> [l]

