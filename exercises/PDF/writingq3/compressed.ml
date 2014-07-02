(* Build a PDF file and write it *)
let _ =
  Pdfwrite.pdf_to_file Pdfhello.hello "compressed.pdf"

