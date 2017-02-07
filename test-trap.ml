open Lineseg
open Linesegintxn
open Printf
open Trapezoid

let wait_click () =
  flush_all ();
  let g=Graphics.wait_next_event [Graphics.Button_down] in ()

let test_split t ls =
  Graphics.clear_graph (); 
  Graphics.set_color Graphics.blue;
  Lineseg.draw ls;
  Graphics.set_color Graphics.yellow;
  let children = Trapezoid.split t ls in
    List.iter Trapezoid.draw (Trapdiag.children_list children);
    wait_click ()

let test1 () =
  let ls1 = ((150.,250.),(270.,130.))
  and ls2 = ((200.,140.),(360.,250.))
  and ls3 = ((200.,30.),(360.,10.)) in
  let t = { upper= Closed ls2;
	    lower= Closed ls3;
	    left= Closed 200.;
	    right= Closed 360.;
	    tl= (200.,140.);
	    tr= (360.,250.);
	    bl= (200.,30.);
	    br= (360.,10.)
	  }
  and t2 = { upper= Open;
	    lower= Closed ls2;
	    left= Closed 200.;
	    right= Closed 360.;
	    tl= (200.,500.);
	    tr= (360.,500.);
	    bl= (200.,140.);
	    br= (360.,250.)
	  }
  in
    (*test_split t ls1;*)
    test_split t2 ls1

let _ = 
  Graphics.open_graph ""; 
  test1 ();
  Graphics.close_graph ()
