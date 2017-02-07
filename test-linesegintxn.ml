open Lineseg
open Linesegintxn
open Printf

let _ =
  let tests = [
    [| ((100.,100.),(200.,90.));((150.,250.),(270.,130.)); ((200.,140.),(360.,250.)) |] ;
    [| ((200.,100.),(400.,100.)); ((100.,200.),(300.,200.)) |] ;
    [| ((150.,250.),(270.,130.)); ((200.,140.),(360.,250.)) |] ;
    [| ((150.,250.),(270.,150.)); ((220.,140.),(360.,220.))|] ;
    [| ((100.,100.),(200.,90.)); ((150.,250.),(270.,150.)) |] ;
    [| ((100.,100.),(200.,90.)); ((300.,200.),(400.,350.)); |] ;
    [| ((100.,100.),(200.,90.)); ((300.,200.),(400.,350.));
       ((150.,250.),(270.,150.)) |]
  ]
  and x = [
  ] in
  let test s =
    begin
      let diag = ConsTrapDiag.construct s in
	printf "*** Constructed trap diagram";
 	flush_all ();
	Graphics.clear_graph ();
	Graphics.set_color Graphics.black;
	Trapdiag.draw diag;
	Graphics.set_color Graphics.red;
	Array.iter Lineseg.draw s;
	let g=Graphics.wait_next_event [Graphics.Button_down] in ()
    end in
    Graphics.open_graph "" ;
    List.iter (fun s ->
 		printf "********************************\n";
		printf "Testing linesegments: ";
		Array.iter (fun x -> Lineseg.print x; printf " | ") s;
		printf "\n";
		test s) tests;
    Graphics.close_graph ()
