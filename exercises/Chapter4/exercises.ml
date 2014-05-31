type input =
  {pos_in : unit -> int;
   seek_in : int -> unit;
   input_char : unit -> char;
   in_channel_length : int}

(* 1 *)
let input_of_array a =
  let pos = ref 0 in
    {pos_in = (fun () -> !pos);
     seek_in =
       (fun p ->
          if p < 0 then raise (Invalid_argument "seek before beginning");
          pos := p);
     input_char =
       (fun () ->
         if !pos > Array.length a - 1
           then raise End_of_file
           else (let c = a.(!pos) in pos := !pos + 1; c));
     in_channel_length = Array.length a}

(* 2 *)
let input_string i n =
  let b = Buffer.create 100 in
    try
      for x = 0 to n - 1 do Buffer.add_char b (i.input_char ()) done;
      Buffer.contents b
    with
      End_of_file -> Buffer.contents b

(* 3 *)
type input2 =
  {pos_in : unit -> int;
   seek_in : int -> unit;
   input_char : unit -> char;
   input_char_opt : unit -> char option;
   in_channel_length : int}

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
   seek_in = seek_in ch;
   input_char = (fun () -> input_char ch);
   input_char_opt =
     (fun () ->
        try Some (input_char ch) with End_of_file -> None);
   in_channel_length = in_channel_length ch}

let input_of_string s =
  let pos = ref 0 in
    {pos_in = (fun () -> !pos);
     seek_in =
       (fun p ->
          if p < 0 then raise (Invalid_argument "seek before beginning");
          pos := p);
     input_char =
       (fun () ->
          if !pos > String.length s - 1
            then raise End_of_file
            else (let c = s.[!pos] in pos := !pos + 1; c));
     input_char_opt =
       (fun () ->
          if !pos > String.length s - 1
            then None
            else (let c = s.[!pos] in pos := !pos + 1; Some c));
     in_channel_length = String.length s}

(* 4 *)
type input3 =
  {pos_in : unit -> int;
   seek_in : int -> unit;
   input_char : unit -> char;
   input_byte : unit -> int;
   in_channel_length : int}

let no_more = -1

let input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
   seek_in = seek_in ch;
   input_char = (fun () -> input_char ch);
   input_byte =
     (fun () ->
        try int_of_char (input_char ch) with End_of_file -> no_more);
   in_channel_length = in_channel_length ch}

let input_of_string s =
  let pos = ref 0 in
    {pos_in = (fun () -> !pos);
     seek_in =
       (fun p ->
          if p < 0 then raise (Invalid_argument "seek before beginning");
          pos := p);
     input_char =
       (fun () ->
          if !pos > String.length s - 1
            then raise End_of_file
            else (let c = s.[!pos] in pos := !pos + 1; c));
     input_byte =
       (fun () ->
          if !pos > String.length s - 1
            then no_more
            else (let c = s.[!pos] in pos := !pos + 1; int_of_char c));
     in_channel_length = String.length s}

(* 5 *)
let single_line_input_of_channel ch =
  {pos_in = (fun () -> pos_in ch);
   seek_in = seek_in ch;
   input_char =
     (fun () ->
        match input_char ch with '\n' -> raise End_of_file | c -> c);
   in_channel_length = in_channel_length ch}

let _ =
  input_string (single_line_input_of_channel stdin) max_int

(* 6 *)
type output =
  {output_char : char -> unit;
   out_channel_length : unit -> int}

let output_of_buffer b =
  {output_char = Buffer.add_char b;
   out_channel_length = fun () -> Buffer.length b}

let build_buffer () =
  let b = Buffer.create 20 in
    let o = output_of_buffer b in
      o.output_char 'A';
      o.output_char 'B';
      o.output_char 'C';
      Buffer.contents b
