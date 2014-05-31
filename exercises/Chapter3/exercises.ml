(* Answers to Records questions *)

(* 1 *)
let x = ref 0

(* x.contents <- 1 *)

(* 2 *)
let string_of_month m =
  match m with
    0 -> "January"
  | 1 -> "February"
  | 2 -> "March"
  | 3 -> "April"
  | 4 -> "May"
  | 5 -> "June"
  | 6 -> "July"
  | 7 -> "August"
  | 8 -> "September"
  | 9 -> "October"
  | 10 -> "November"
  | 11 -> "December"
  | _ -> raise (Invalid_argument "string_of_month")

let string_of_day d =
  match d with
    0 -> "Sunday"
  | 1 -> "Monday"
  | 2 -> "Tuesday"
  | 3 -> "Wednesday"
  | 4 -> "Thursday"
  | 5 -> "Friday"
  | 6 -> "Saturday"
  | _ -> raise (Invalid_argument "string_of_day")

let string_of_time () =
  let
    {Unix.tm_min;
     Unix.tm_hour;
     Unix.tm_mday;
     Unix.tm_mon;
     Unix.tm_year;
     Unix.tm_wday}
  =
    Unix.localtime (Unix.time ())
  in
      "It is " 
    ^ string_of_int tm_hour
    ^ ":"
    ^ string_of_int tm_min
    ^ " on "
    ^ string_of_day tm_wday
    ^ " "
    ^ string_of_int tm_mday
    ^ " "
    ^ string_of_month tm_mon
    ^ " "
    ^ string_of_int (tm_year + 1900)


(* 4 *)
type ('a, 'b, 'c) t =
  {a : 'a;
   b : 'a;
   c : 'b;
   d : 'b;
   e : 'c;
   f : 'c}

(* 5 (a) *)
let write_gc_summary filename =
  let ch = open_out filename in
    let
      {Gc.minor_words;
       Gc.promoted_words;
       Gc.major_words;
       Gc.minor_collections;
       Gc.major_collections}
    =
      Gc.stat ()
    in
      output_string ch "Minor Words: ";
      output_string ch (string_of_float minor_words);
      output_string ch "\nPromoted Words: ";
      output_string ch (string_of_float promoted_words);
      output_string ch "\nMajor Words: ";
      output_string ch (string_of_float major_words);
      output_string ch "\nMinor Collections: ";
      output_string ch (string_of_int minor_collections);
      output_string ch "\nMajor Collections: ";
      output_string ch (string_of_int major_collections);
      close_out ch

(* 5 (b) *)
let start_of_major = 0x001
and minor_collection = 0x002
and heap_grow_shrink = 0x004
and stack_resizing = 0x008
and heap_compaction = 0x010
and change_parameters = 0x020
and compute_slice_size = 0x040
and call_finalisation = 0x080
and bytecode_exe_search = 0x100

let change_verbosity vs =
  let n = 
    List.fold_left ( + ) 0 vs
  in
    Gc.set {(Gc.get ()) with Gc.verbose = n}

