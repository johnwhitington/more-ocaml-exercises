open More

let basic_document str =
  let objects =
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
                          ("/BaseFont", Pdf.Name "/Courier")])])]);
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

let hello_text =
  "1 0 0 1 50 770 cm BT /F0 36 Tf (Hello, World!) Tj ET"

let _ =
  Pdfwrite.pdf_to_file (basic_document hello_text) "pdfwriter.pdf"

