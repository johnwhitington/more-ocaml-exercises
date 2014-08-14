(* Printf examples *)
type 'a point =
  {x : float;
   y : float;
   label : string;
   content : 'a}

let p =
  {x = 4.5;
   y = 6.0;
   label = "P";
   content = [1; 3; 1]}

let string_of_point p =
  p.label
  ^ " = ("
  ^ string_of_float p.x
  ^ ", "
  ^ string_of_float p.y
  ^ ")"

let string_of_point p =
  Printf.sprintf "%s = (%f, %f)" p.label p.x p.y

(* Integers in columns *)
let data =
  [(1, 6, 5);
   (2, 18, 4);
   (3, 31, 12);
   (4, 16, 2)]

let print_header () =
  Printf.printf "A     | B     | C     \n";
  Printf.printf "------+-------+-------\n"

let print_line a b c =
  Printf.printf "%6i| %6i| %6i\n" a b c

let print_nums nums =
  print_header ();
  List.iter (fun (a, b, c) -> print_line a b c) nums

let _ = print_nums data

(* Things in columns, with flags and precisions. *)
let data =
  [(1, 35.54263, 39.42312);
   (2, 12.31341, 23.24123);
   (3, 13.53342, 23.21457);
   (4, 57.74572, 126.74554)]

let print_header () =
  Printf.printf "A     | B     | C     \n";
  Printf.printf "------+-------+-------\n"

let print_line a b c =
  Printf.printf "%06i| %-6.2f| %-6.2f\n" a b c

let print_nums nums =
  print_header ();
  List.iter (fun (a, b, c) -> print_line a b c) nums

let print_point p =
  Printf.printf "%s = (%f, %f)%!" p.label p.x p.y

let _ = print_nums data

