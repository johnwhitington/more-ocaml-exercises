(* Build PDF page, with ops. *)

type t =
    Move of float * float (* Move *)
  | Line of float * float (* Line *)
  | Close (* close subpath *)
  | Stroke (* Stroke *)
  | Fill (* fill non-zero *)
  | FillColour of float (* set fill colour *)
  | StrokeColour of float (* set stroke colour *)

  | SetClip
  | StrokeColourRGB of float * float * float
  | FillColourRGB of float * float * float
  | LineWidth of float

  | BeginText (* Op_BT *)
  | EndText (* Op_ET *)
  | SetTextPosition of float * float (* Op_Tm *)
  | SetFontAndSize of string * float
  | ShowText of string

  | SetCharacterSpacing of float

let string_of_op op =
  match op with
    Move (x, y) -> Printf.sprintf "%f %f m" x y
  | Line (x, y) -> Printf.sprintf "%f %f l" x y
  | Close -> "h"
  | Stroke -> "S"
  | Fill -> "f"
  | FillColour g -> Printf.sprintf "%f g" g
  | StrokeColour g -> Printf.sprintf "%f G" g
 
  | SetClip -> "W" 
  | StrokeColourRGB (r, g, b) -> Printf.sprintf "%f %f %f RG" r g b
  | FillColourRGB (r, g, b) -> Printf.sprintf "%f %f %f rg" r g b
  | LineWidth w -> Printf.sprintf "%f w" w


  | BeginText -> "BT"
  | EndText -> "ET"
  | SetTextPosition (x, y) -> Printf.sprintf "1 0 0 1 %f %f Tm" x y
  | SetFontAndSize (font, size) -> Printf.sprintf "%s %f Tf" font size
  | ShowText t -> Printf.sprintf "(%s) Tj" t

  | SetCharacterSpacing s -> Printf.sprintf "%f Tc" s
  
let string_of_ops ops =
  let b = Buffer.create 1000 in
    List.iter
      (fun op ->
         Buffer.add_string b (string_of_op op);
         Buffer.add_char b ' ')
      ops;
    Buffer.contents b

