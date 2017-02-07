
open Getopt

let inputfn = ref ""
and outputfn = ref ""

let specs = 
[
  ( 'i', "input",  None, (atmost_once inputfn (Error "only one input")));
  ( 'o', "output",  None, (atmost_once outputfn (Error "only one output")));
]

let main() =
  print_string "line segment intersections 0.1\n";

  parse_cmdline specs print_endline;

  (* read input file *)

  (* find intersecting line segments *)

  (* write output file *)

  exit 0;;

main();;


