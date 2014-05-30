(* Circles. The questions. *)
let basic_document page_contents =
  let str = Pdfpage.string_of_ops page_contents in
    let objects =
      [(1,
         Pdf.Dictionary
           [("/Type", Pdf.Name "/Page");
            ("/Parent", Pdf.Indirect 3);
            ("/MediaBox",
                Pdf.Array
                  [Pdf.Float 0.; Pdf.Float 0.;
                   Pdf.Float 595.275590551; Pdf.Float 841.88976378]);
            ("/Rotate", Pdf.Integer 0);
            ("/Contents", Pdf.Array [Pdf.Indirect 4])]);
       (2,
         Pdf.Dictionary
          [("/Type", Pdf.Name "/Catalog");
           ("/Pages", Pdf.Indirect 3)]);
       (3,
         Pdf.Dictionary
          [("/Type", Pdf.Name "/Pages");
           ("/Kids", Pdf.Array [Pdf.Indirect 1]);
           ("/Count", Pdf.Integer 1)]);
       (4,
         Pdf.Stream
           (Pdf.Dictionary [("/Length", Pdf.Integer (String.length str))], str))]
    in
      {Pdf.version = (1, 1);
       Pdf.objects = objects;
       Pdf.trailer = Pdf.Dictionary [("/Size", Pdf.Integer 5); ("/Root", Pdf.Indirect 2)]}

(* Q 1. Build a circle. *)
let pi = 4. *. atan 1.

(* Point at a given angle *)
let point x y r angle =
  (cos angle *. r +. x, sin angle *. r +. y)

(* Points around a circle with a step in radians *)
let points x y r step =
  let n = ref 0
  and points = ref [] in
    while float_of_int !n *. step < 2. *. r *. pi do
      points := point x y r (float_of_int !n *. step) :: !points;
      n := !n + 1
    done;
    List.rev !points

(* Build a path representing a circle *)
let circle x y r =
  match points x y r (pi /. 20.) with
     (x, y) :: lines ->
       Pdfpage.Move (x, y) :: List.map (fun (x, y) -> Pdfpage.Line (x, y)) lines @ [Pdfpage.Close]
   | _ ->
       assert false 

let circle_filled =
  circle 300. 300. 100. @ [Pdfpage.Fill]

let _ =
  Pdfwrite.pdf_to_file (basic_document circle_filled) "q1.pdf"

(* Q 2. Page covered in random circles. *)
let random_circle () =
  let x = Random.float 700. -. 50.
  and y = Random.float 1000. -. 50.
  and r = Random.float 100. +. 20. in
    circle x y r

let rec many f n = 
  match n with
    0 -> []
  | _ -> f () :: many f (n - 1)

let many_circles =
  List.concat
    (List.map
      (fun l -> List.append (Pdfpage.FillColour (Random.float 1.) :: l) [Pdfpage.Fill])
      (many random_circle 100))

let _ =
  Pdfwrite.pdf_to_file (basic_document many_circles) "q2.pdf"
  
(* Q 3. Coloured circles *)
let many_circles_colour =
  List.concat
    (List.map
      (fun l -> List.append (Pdfpage.FillColourRGB (Random.float 1., Random.float 1., Random.float 1.) :: l) [Pdfpage.Fill])
      (many random_circle 100))

let _ =
  Pdfwrite.pdf_to_file (basic_document many_circles_colour) "q3.pdf"

(* Q 4. Single with width. *)
let big_circle =
  [Pdfpage.LineWidth 5.; Pdfpage.StrokeColour 0.] @ circle 300. 400. 150. @ [Pdfpage.Stroke]

let circle_over_circles =
  many_circles_colour @ big_circle

let _ =
  Pdfwrite.pdf_to_file (basic_document circle_over_circles) "q4.pdf"

(* Q 5. Clipping *)
let big_clipping_circle =
  [Pdfpage.LineWidth 1.; Pdfpage.StrokeColour 0.] @
  circle 300. 400. 150. @
  [Pdfpage.SetClip; Pdfpage.Stroke]

let clipped =
  big_clipping_circle @ many_circles_colour

let _ =
  Pdfwrite.pdf_to_file (basic_document clipped) "q5.pdf"
