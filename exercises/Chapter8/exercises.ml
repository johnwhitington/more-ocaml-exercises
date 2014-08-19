(* 1 *)
let rec cycle_of_points_inner b l =
  match l with
    [] ->
      Buffer.contents b
  | [(x, y)] ->
      Printf.bprintf b "(%i, %i)" x y;
      Buffer.contents b
  | (x, y)::t ->
      Printf.bprintf b "(%i, %i) --> " x y;
      cycle_of_points_inner b t

let cycle_of_points l =
  match l with
    [] -> ""
  | h::t ->
      cycle_of_points_inner (Buffer.create 256) ((h :: t) @ [h])

(* 2 *)
let hex_of_string s = 
  let b = Buffer.create (String.length s * 2) in
    String.iter
      (fun c -> Printf.bprintf b "%02X" (int_of_char c))
      s;
    Buffer.contents b

(* 3 *)
let mkstring () = "string"

let exercise_3 () = Printf.printf "%s" (mkstring ())

(* 4 *)
let exercise_4 () = Printf.sprintf "(%*i)" 10 1

let print_integers w ns =
  List.iter (Printf.printf "(%*i)" w) ns

