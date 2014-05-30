(* Representing PDF Documents *)
type pdfobject =
  | Boolean of bool
  | Integer of int
  | Float of float
  | String of string
  | Name of string 
  | Array of pdfobject list
  | Dictionary of (string * pdfobject) list
  | Stream of pdfobject * string
  | Indirect of int

type t =
  {version : int * int; 
   objects : (int * pdfobject) list; 
   trailer : pdfobject}

