(* text *)
open More

let page_width = 600.

let page_height = 800.

let font_size = 10.0

let line_spacing = 1.1

let margin = 50.0

let text_width = page_width -. margin -. margin

let max_chars = int_of_float (text_width /. font_size *. (5. /. 3.))

let basic_document page_contents =
  let str = Pdfpage.string_of_ops page_contents in
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
                   Pdf.Float page_width; Pdf.Float page_height]);
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


let metamorphosis = "One morning, when Gregor Samsa woke from troubled dreams, he found himself transformed in his bed into a horrible vermin. He lay on his armour-like back, and if he lifted his head a little he could see his brown belly, slightly domed and divided by arches into stiff sections. The bedding was hardly able to cover it and seemed ready to slide off any moment. His many legs, pitifully thin compared with the size of the rest of him, waved about helplessly as he looked.
\"What's happened to me?\" he thought. It wasn't a dream. His room, a proper human room although a little too small, lay peacefully between its four familiar walls. A collection of textile samples lay spread out on the table - Samsa was a travelling salesman - and above it there hung a picture that he had recently cut out of an illustrated magazine and housed in a nice, gilded frame. It showed a lady fitted out with a fur hat and fur boa who sat upright, raising a heavy fur muff that covered the whole of her lower arm towards the viewer.
Gregor then turned to look out the window at the dull weather. Drops of rain could be heard hitting the pane, which made him feel quite sad. \"How about if I sleep a little bit longer and forget all this nonsense\", he thought, but that was something he was unable to do because he was used to sleeping on his right, and in his present state couldn't get into that position. However hard he threw himself onto his right, he always rolled back to where he was. He must have tried it a hundred times, shut his eyes so that he wouldn't have to look at the floundering legs, and only stopped when he began to feel a mild, dull pain there that he had never felt before.
\"Oh, God\", he thought, \"what a strenuous career it is that I've chosen! Travelling day in and day out. Doing business like this takes much more effort than doing your own business at home, and on top of that there's the curse of travelling, worries about making train connections, bad and irregular food, contact with different people all the time so that you can never get to know anyone or become friendly with them. It can all go to Hell!\" He felt a slight itch up on his belly; pushed himself slowly up on his back towards the headboard so that he could lift his head better; found where the itch was, and saw that it was covered with lots of little white spots which he didn't know what to make of; and when he tried to feel the place with one of his legs he drew it quickly back because as soon as he touched it he was overcome by a cold shudder.
He slid back into his former position. \"Getting up early all the time\", he thought, \"it makes you stupid. You've got to get enough sleep. Other travelling salesmen live a life of luxury. For instance, whenever I go back to the guest house during the morning to copy out the contract, these gentlemen are always still sitting there eating their breakfasts. I ought to just try that with my boss; I'd get kicked out on the spot. But who knows, maybe that would be the best thing for me. If I didn't have my parents to think about I'd have given in my notice a long time ago, I'd have gone up to the boss and told him just what I think, tell him everything I would, let him know just what I feel. He'd fall right off his desk! And it's a funny sort of business to be sitting up there at your desk, talking down at your subordinates from up there, especially when you have to go right up close because the boss is hard of hearing. Well, there's still some hope; once I've got the money together to pay off my parents' debt to him - another five or six years I suppose - that's definitely what I'll do. That's when I'll make the big change. First of all though, I've got to get up, my train leaves at five.\""

(* Read until a space or end of intput, consuming the spaces. \n will be read as
 * a 'word' on its own. *)
let consume_spaces i =
  try
    while true do
      match i.Input.input_char () with
        ' ' -> ()
      | x -> Input.rewind i; raise End_of_file
    done;
    assert false
  with
    End_of_file -> ()

let read_word i =
  consume_spaces i;
  let b = Buffer.create 20 in
    try
      while true do
        match i.Input.input_char () with
          '\n' ->
            if Buffer.length b = 0 then Buffer.add_char b '\n' else Input.rewind i;
            raise End_of_file
        | ' ' -> raise End_of_file
        | c -> Buffer.add_char b c
        done;
      assert false
    with
      End_of_file ->
        if Buffer.length b = 0
          then raise End_of_file
          else Buffer.contents b

let words_of_input i =
  let words = ref [] in
    try
      while true do words := read_word i :: !words done;
      assert false
    with
      End_of_file -> List.rev !words

(* Full lines are all but last *)
type line =
    Full of string
  | Partial of string

let rec lines_inner ls b width indent words =
  match words with
    [] ->
      if Buffer.length b > 0 then
        List.rev (Partial (Buffer.contents b) :: ls)
      else
        List.rev ls
  | "\n"::t ->
      let b' = Buffer.create width in
        for x = 1 to indent do Buffer.add_char b' ' ' done;
        lines_inner
          (Partial (Buffer.contents b) :: ls) b' width indent t
  | word::t ->
      if Buffer.length b = 0 && String.length word > width then
        lines_inner (Full word :: ls) (Buffer.create width) width indent t
      else if String.length word + Buffer.length b < width then
        begin
          Buffer.add_string b word;
          if Buffer.length b < width then Buffer.add_char b ' ';
          lines_inner ls b width indent t
        end
      else
        lines_inner
          (Full (Buffer.contents b) :: ls)
          (Buffer.create width) width indent (word :: t)

let lines width ?(indent=0) words =
  lines_inner [] (Buffer.create width) width indent words

(* Typeset a single line at given y coordinate *)
let typeset_line_at line y =
  [Pdfpage.BeginText;
   Pdfpage.SetTextPosition (margin, y);
   Pdfpage.SetFontAndSize ("/F0", font_size);
   Pdfpage.ShowText line;
   Pdfpage.EndText]

let rec downfrom step start length n =
  if length = 0 then [] else
    start -. (step *. float n) :: downfrom step start (length - 1) (n + 1)

let clean_lines =
  List.map (function Full x -> x | Partial x -> x)

(* Typeset a single page of text, assuming the correct number of lines *)
let typeset_page text =
  let words = words_of_input (Input.input_of_string text) in
    let ls = clean_lines (lines max_chars ~indent:8 words) in
      let positions =
        downfrom
          (font_size *. line_spacing)
          (page_height -. margin -. line_spacing) (List.length ls) 0
      in
        List.concat (List.map2 typeset_line_at ls positions)

let _ =
  Pdfwrite.pdf_to_file
    (basic_document (typeset_page metamorphosis))
    "text.pdf"

