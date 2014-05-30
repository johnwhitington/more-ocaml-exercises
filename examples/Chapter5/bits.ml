(* Bit streams *)
open Input

type input_bits =
  {input : input;
   mutable byte : int;
   mutable bit : int}

let input_bits_of_input i =
  {byte = 0;
   bit = 0;
   input = i}

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

let getval_32 b n =
  if n < 0 then raise (Invalid_argument "getval_32") else
    if n = 0 then 0l else
      let r = ref Int32.zero in
        for x = n - 1 downto 0 do
          r := Int32.logor !r (Int32.shift_left (Int32.of_int ((if getbit b then 1 else 0))) x)
        done;
        !r

(* Deconstruction of TCP datagram header *)
let print_header filename =
  let ch = open_in_bin filename in
  let i = input_of_channel ch in
  let b = input_bits_of_input i in
    let src_port = getval b 16 in
    let dest_port = getval b 16 in
    let seq_port = getval_32 b 32 in
    let ack_number = getval_32 b 32 in
    let _ = getval b 4 in (* data offset *)
    let _ = getval b 6 in (* reserved *)
    let urgent = getbit b in
    let ack = getbit b in
    let push = getbit b in
    let reset = getbit b in
    let syn = getbit b in
    let fin = getbit b in
    let receive = getval b 16 in
    let checksum = getval b 16 in
    let urgent_pointer = getval b 16 in
      Printf.printf
        "Source port = %i, Destination = %i, Sequence = %li, Acknowledgement Number = %li\n"
        src_port dest_port seq_port ack_number;
      Printf.printf
        "Flags: Urgent = %B, Ack = %B, Push = %B, Reset = %B, Syn = %B, Fin = %B\n"
        urgent ack push reset syn fin;
      Printf.printf
        "Receive window size = %i, checksum = %i, urgent pointer = %i\n"
        receive checksum urgent_pointer;
      close_in ch
       
(* Output bit streams *)
type output_bits =
  {output : output; (* underlying output *)
   mutable obyte : int; (* the byte we're building up *)
   mutable obit : int}

let output_bits_of_output output =
  {output;
   obyte = 0;
   obit = 0}

(* Flush a byte to the underlying output, padding with zeroes. If output byte
 * has not been touched, don't output. *)
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

(* Output bit streams example. *)
let output_header filename =
  let ch = open_out_bin filename in
  let o = output_of_channel ch in
  let bits = output_bits_of_output o in
    putval bits 110 16;
    putval bits 1183 16;
    putval bits 1952181922 32;
    putval bits 1503264782 32;
    putval bits 5 4;
    putval bits 0 6;
    putbit bits 0;
    putbit bits 1;
    putbit bits 1;
    putbit bits 0;
    putbit bits 0;
    putbit bits 0;
    putval bits 9216 16;
    putval bits 58154 16;
    putval bits 0 16;
    close_out ch
