let box =
  [Pdfpage.Move (100., 100.);
   Pdfpage.Line (100., 200.);
   Pdfpage.Line (200., 200.);
   Pdfpage.Line (200., 100.);
   Pdfpage.Close;
   Pdfpage.Fill]

let hello_text =
  "1 0 0 1 50 770 cm BT /F0 36 Tf (Hello, World!) Tj ET"

let box_string =
  Pdfpage.string_of_ops box

let objects str =
  [(1,
     Pdf.Dictionary
       [("/Type", Pdf.Name "/Page");
        ("/Parent", Pdf.Indirect 3);
        ("/Resources",
           Pdf.Dictionary
             [("/Font",
                 Pdf.Dictionary
                   [("/F0",
                     Pdf.Dictionary
                       [("/Type", Pdf.Name "/Font");
                        ("/Subtype", Pdf.Name "/Type1");
                        ("/BaseFont", Pdf.Name "/Times-Italic")])])]);
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

let hello =
  {Pdf.version = (1, 1);
   Pdf.objects = objects box_string;
   Pdf.trailer =
     Pdf.Dictionary
       [("/Size", Pdf.Integer 5);
        ("/Root", Pdf.Indirect 2);
        ("/ID", Pdf.Array [Pdf.String "FIXME"; Pdf.String "FIXME"])]}

let append a b =
  List.rev_append (List.rev a) b

let ( @ ) = append

let map f l =
  List.rev (List.rev_map f l)

let mkline x y x1 y1 =
  [Pdfpage.Move (x, y);
   Pdfpage.Line (x1, y1);
   Pdfpage.Stroke]

let steps n step =
  let mul = ref 0
  and vals = ref [] in
    while float !mul *. step <= n do
      vals := float !mul *. step :: !vals;
      mul := !mul + 1
    done;
    List.rev !vals

let graph_string w h step =
  let horizontals =
    map (fun n -> mkline 0. n w n) (steps h step)
  and verticals =
    map (fun n -> mkline n 0. n h) (steps w step)
  in
    Pdfpage.string_of_ops (List.concat (horizontals @ verticals))

let graph =
  {Pdf.version = (1, 1);
   Pdf.objects = objects (graph_string 595.275590551 841.88976378 10.);
   Pdf.trailer =
     Pdf.Dictionary
       [("/Size", Pdf.Integer 5);
        ("/Root", Pdf.Indirect 2);
        ("/ID", Pdf.Array [Pdf.String "FIXME"; Pdf.String "FIXME"])]}

