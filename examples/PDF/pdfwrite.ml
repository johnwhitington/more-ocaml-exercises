let rec string_of_array a =
  let b = Buffer.create 100 in
    Buffer.add_string b "[";
    List.iter
      (fun s ->
         Buffer.add_char b ' ';
         Buffer.add_string b (string_of_pdfobject s))
      a;
    Buffer.add_string b " ]";
    Buffer.contents b

and string_of_dictionary d =
  let b = Buffer.create 100 in
    Buffer.add_string b "<<";
    List.iter
      (fun (k, v) ->
         Buffer.add_char b ' ';
         Buffer.add_string b k;
         Buffer.add_char b ' ';
         Buffer.add_string b (string_of_pdfobject v))
      d;
    Buffer.add_string b " >>";
    Buffer.contents b

and string_of_stream dict data =
  let b = Buffer.create 100 in
    List.iter (Buffer.add_string b)
      [(string_of_pdfobject dict); "\nstream\n"; data; "\nendstream"];
    Buffer.contents b

and string_of_pdfobject obj =
  match obj with
    Pdf.Boolean b -> string_of_bool b
  | Pdf.Integer i -> string_of_int i
  | Pdf.Float f -> string_of_float f
  | Pdf.String s -> "(" ^ s ^ ")"
  | Pdf.Name n -> n
  | Pdf.Array a -> string_of_array a
  | Pdf.Dictionary d -> string_of_dictionary d
  | Pdf.Stream (dict, data) -> string_of_stream dict data
  | Pdf.Indirect i -> Printf.sprintf "%i 0 R" i

let write_header o {Pdf.version = (major, minor)} =
  output_string o
    (Printf.sprintf "%%PDF-%i.%i\n%%\150\150\150\150\n" major minor)

let write_trailer o pdf offsets =
  let startxref = pos_out o in
    output_string o "xref\n";
    output_string o (Printf.sprintf "0 %i\n" (List.length pdf.Pdf.objects + 1));
    output_string o "0000000000 65535 f \n";
    List.iter
      (fun offset ->
         output_string o (Printf.sprintf "%010i 00000 n \n" offset))
      offsets;
    output_string o "trailer\n";
    output_string o (string_of_pdfobject pdf.Pdf.trailer);
    output_string o "\nstartxref\n";
    output_string o (string_of_int startxref);
    output_string o "\n%%EOF"

(* Write the objects, in sorted order. They are not renumbered in any way, so
 * the table in the trailer might have null entries. *)
let write_objects o objs =
  let objs' = List.sort compare objs in
    let offsets = ref [] in
      List.iter
        (fun (objnum, obj) ->
           offsets := pos_out o :: !offsets;
           output_string o (Printf.sprintf "%i 0 obj\n" objnum);
           output_string o (string_of_pdfobject obj);
           output_string o "\nendobj\n")
        objs';
      List.rev !offsets

let pdf_to_file pdf filename =
  let output = open_out_bin filename in
    try
      write_header output pdf;
      let offsets = write_objects output pdf.Pdf.objects in
        write_trailer output pdf offsets;
        close_out output
    with
      e -> close_out output; raise e

