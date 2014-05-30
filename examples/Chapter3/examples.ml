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

let mkpoint x y l c =
  {x = x; y = y; label = l; content = c}

let mkpoint x y label content =
  {x; y; label; content}

let string_of_point p =
  p.label
  ^ " = ("
  ^ string_of_float p.x
  ^ ", "
  ^ string_of_float p.y
  ^ ")"

let string_of_point {label = l; x = x; y = y} =
  l
  ^ " = ("
  ^ string_of_float x
  ^ ", "
  ^ string_of_float y
  ^ ")"

let string_of_point {label = l; x = x; y = y; _} =
  l
  ^ " = ("
  ^ string_of_float x
  ^ ", "
  ^ string_of_float y
  ^ ")"

let string_of_point {label; x; y; _} =
  label
  ^ " = ("
  ^ string_of_float x
  ^ ", "
  ^ string_of_float y
  ^ ")"

let relabel p l = {p with label = l}

let relabel p label = {p with label}

let mirror p = {p with x = p.y; y = p.x}




