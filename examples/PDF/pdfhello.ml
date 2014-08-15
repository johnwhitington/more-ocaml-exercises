let box =
  [Pdfpage.Move (100., 100.);
   Pdfpage.Line (100., 200.);
   Pdfpage.Line (200., 200.);
   Pdfpage.Line (200., 100.);
   Pdfpage.Close;
   Pdfpage.Fill]

let hello_text =
  "1 0 0 1 50 770 cm BT /F0 36 Tf (Hello, World!) Tj ET"

let hello_text2 =
  "1 0 0 1 50 770 cm BT /F0 36 Tf (Hello, World 2!) Tj ET"

let hello_text3 =
  "1 0 0 1 50 770 cm BT /F0 36 Tf (Hello, World 3!) Tj ET"

(*let box_string =
  Pdfpage.string_of_ops box*)

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
   Pdf.objects = objects hello_text;
   Pdf.trailer =
     Pdf.Dictionary
       [("/Size", Pdf.Integer 5);
        ("/Root", Pdf.Indirect 2);
        ("/ID", Pdf.Array [Pdf.String "FIXME"; Pdf.String "FIXME"])]}

let objects3 str str2 str3 =
  [(1,
     Pdf.Dictionary
       [("/Type", Pdf.Name "/Page");
        ("/Parent", Pdf.Indirect 3);
        ("/Resources", Pdf.Indirect 5);
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
       ("/Kids", Pdf.Array [Pdf.Indirect 1; Pdf.Indirect 6; Pdf.Indirect 7]);
       ("/Count", Pdf.Integer 3)]);
   (4,
     Pdf.Stream
       (Pdf.Dictionary [("/Length", Pdf.Integer (String.length str))], str));
   (5,
     Pdf.Dictionary
       [("/Font",
           Pdf.Dictionary
             [("/F0",
               Pdf.Dictionary
                 [("/Type", Pdf.Name "/Font");
                  ("/Subtype", Pdf.Name "/Type1");
                  ("/BaseFont", Pdf.Name "/Times-Italic")])])]);
    (6,
       Pdf.Dictionary
         [("/Type", Pdf.Name "/Page");
          ("/Parent", Pdf.Indirect 3);
          ("/Resources", Pdf.Indirect 5);
          ("/MediaBox",
              Pdf.Array
                [Pdf.Float 0.; Pdf.Float 0.;
                 Pdf.Float 595.275590551; Pdf.Float 841.88976378]);
          ("/Rotate", Pdf.Integer 0);
          ("/Contents", Pdf.Array [Pdf.Indirect 8])]);
    (7,
       Pdf.Dictionary
         [("/Type", Pdf.Name "/Page");
          ("/Parent", Pdf.Indirect 3);
          ("/Resources", Pdf.Indirect 5);
          ("/MediaBox",
              Pdf.Array
                [Pdf.Float 0.; Pdf.Float 0.;
                 Pdf.Float 595.275590551; Pdf.Float 841.88976378]);
          ("/Rotate", Pdf.Integer 0);
          ("/Contents", Pdf.Array [Pdf.Indirect 9])]);
   (8,
     Pdf.Stream
       (Pdf.Dictionary [("/Length", Pdf.Integer (String.length str2))], str2));
   (9,
     Pdf.Stream
       (Pdf.Dictionary [("/Length", Pdf.Integer (String.length str3))], str3))]

let hello3 =
  {Pdf.version = (1, 1);
   Pdf.objects = objects3 hello_text hello_text2 hello_text3;
   Pdf.trailer =
     Pdf.Dictionary
       [("/Size", Pdf.Integer 10);
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

(* Consider the tree Br (Br (Lf, 1, Lf), 2, Br (Lf, 3, Lf)). We will represent it as:

  <</Type /Br
    /Value 2
    /Left
       <</Type /Br /Value 1 /Left /Lf /Right /Lf>>
    /Right
      << /Type /Br /Value 3 /Left /Lf /Right /Lf>> 
  >>
*)
let tree =
  Pdf.Dictionary
    [("/Type", Pdf.Name "/Br");
     ("/Value", Pdf.Integer 2);
     ("/Left",
        Pdf.Dictionary
          [("/Type", Pdf.Name "/Br");
           ("/Value", Pdf.Integer 1);
           ("/Left", Pdf.Name "/Lf");
           ("/Right", Pdf.Name "/Lf")]);
     ("/Right",
         Pdf.Dictionary
           [("/Type", Pdf.Name "/Br");
            ("/Value", Pdf.Integer 3);
            ("/Left", Pdf.Name "/Lf");
            ("/Right", Pdf.Name "/Lf")])]

let rec rotate_90 obj =
  match obj with
    Pdf.Array objs -> Pdf.Array (List.map rotate_90 objs)
  | Pdf.Dictionary objs -> Pdf.Dictionary (List.map rotate_90_dict objs)
  | Pdf.Stream (dict, str) -> Pdf.Stream (rotate_90 dict, str)
  | x -> x 

and rotate_90_dict (k, v)  =
  match k with
    "/Rotate" -> ("/Rotate", Pdf.Integer 90)
  | _ -> (k, rotate_90 v)



