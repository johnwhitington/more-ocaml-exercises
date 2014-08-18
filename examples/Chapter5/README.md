Examples for Chapter 5
======================

These examples can be explored either by copying-and-pasting into the OCaml
top-level, or by the prepared top level "bits.top":

        OCaml

# Bits.input_example ();;
Source port = 38
Destination = 47892
Sequence = 1656212531
Acknowledgement Number = 1481973485
data offset = 0
reserved = 32
Flags:
 Urgent = false
 Ack = false
 Push = false
 Reset = false
 Syn = false
 Fin = false
Receive window size = 17664
Checksum = 888
Urgent pointer = 63404
- : unit = ()

The resulting 'bits' library built in this directory is also used by several
of the other chapters' examples and exercises.

