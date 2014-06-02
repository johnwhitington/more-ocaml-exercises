(* Answers for chapter 5 *)

type input =
  {pos_in : unit -> int;
   seek_in : int -> unit;
   input_char : unit -> char;
   in_channel_length : int}

type input_bits =
  {input : input;
   mutable byte : int;
   mutable bit : int}

let rec getbit b =
  if b.bit = 0 then
    begin
      b.byte <- int_of_char (b.input.input_char ());
      b.bit <- 128;
      getbit b
    end
  else
    let r = b.byte land b.bit > 0 in
      b.bit <- b.bit / 2;
      r
     
let align b =
  b.bit <- 0

let getval b n =
  if n <= 0 || n > 31 then
    raise (Invalid_argument "getval")
  else
    let r = ref 0 in
      for x = n - 1 downto 0 do
        r := !r lor ((if getbit b then 1 else 0) lsl x)
      done;
      !r

(* 1 *)
let getval_fast b n =
  if n = 8 && b.bit = 0
    then int_of_char (b.input.input_char ())
    else getval b n

(* 2 *)
let getval_32 b n =
  if n < 0 then raise (Invalid_argument "getval_32") else
    if n = 0 then 0l else
      let r = ref Int32.zero in
        for x = n - 1 downto 0 do
          r := Int32.logor !r (Int32.shift_left (Int32.of_int ((if getbit b then 1 else 0))) x)
        done;
        !r

(* 3 *)
type output =
  {output_char : char -> unit;
   out_channel_length : unit -> int}

type output_bits =
  {output : output; (* underlying output *)
   mutable obyte : int; (* the byte we're building up *)
   mutable obit : int}

let flush o =
  if o.obit < 7 then o.output.output_char (char_of_int o.obyte);
  o.obyte <- 0;
  o.obit <- 7

let rec putbit o b =
  if o.obit = -1 then
    begin
      flush o;
      putbit o b
    end
  else
    begin
      if b = 1 then o.obyte <- o.obyte lor (1 lsl o.obit);
      o.obit <- o.obit - 1
    end

let putval o v l =
  for x = l - 1 downto 0 do
    putbit o (v land (1 lsl x))
  done

let putval_fast o v l =
  if l = 8 && o.obit = 7
    then o.output.output_char (char_of_int v)
    else putval o v l
    
(* 4 FIXME *)
let putval_32 o v l =
  for x = l - 1 downto 0 do
    putbit o (Int32.to_int (Int32.logand v (Int32.shift_left (Int32.of_int x) 1)))
  done

(* 5 *)
type output2 =
  {output_char : char -> unit;
   rewind : unit -> unit;
   out_channel_length : unit -> int}

let output_of_string s =
  let pos = ref 0 in
    {output_char =
       (fun c ->
          if !pos < String.length s
            then (s.[!pos] <- c; pos := !pos + 1)
            else raise End_of_file);
     rewind =
       (fun () ->
          if !pos > 0
            then pos := !pos - 1
            else raise (Failure "rewind"));
     out_channel_length =
       (fun () -> String.length s)}

type output_bits2 =
  {output : output2; 
   mutable obyte : int;
   mutable obit : int}

let output_bits_of_output output =
  {output;
   obyte = 0;
   obit = 0}
  
let rec putbit o b =
  if o.obit = -1 then
    begin
      o.obyte <- 0;
      o.obit <- 7;
      putbit o b
    end
  else
    begin
      if b = 1 then o.obyte <- o.obyte lor (1 lsl o.obit);
      o.output.output_char (char_of_int o.obyte);
      o.output.rewind ();
      o.obit <- o.obit - 1
    end

