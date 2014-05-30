(* Build PDF page, with ops *)
type t =
    Move of float * float
  | Line of float * float
  | Close
  | Stroke
  | Fill
  | FillColour of float
  | StrokeColour of float

  | SetClip
  | StrokeColourRGB of float * float * float
  | FillColourRGB of float * float * float
  | LineWidth of float

  | BeginText
  | EndText
  | SetTextPosition of float * float
  | SetFontAndSize of string * float
  | ShowText of string

  | SetCharacterSpacing of float

val string_of_op : t -> string

val string_of_ops : t list -> string

