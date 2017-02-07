(*
**
** ocaml module Linesegintxn
**
** Description: Computes the trapezoidal map of a set of line segments
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Lineseg
open Printf

let wait_key () =
  let g = Graphics.wait_next_event [Graphics.Button_down] in
    flush_all ()


module LineSegIntxn =
struct
  type obj = lineseg
  type region = Trapezoid.trapezoid
  type region_set = Trapdiag.trapdiag

  let init_regionset () =
    let t = Trapdiag.empty_trapdiag () in
    let i = Trapdiag.add_trap Trapezoid.empty_trap t in
      t
	
  let intersect ls trap = Trapezoid.intersect trap ls

(*
  let partition objects regions =	(* partition objects over regions *)
*)  
    
  let sort_traps td l =
    let bnd x = (Trapezoid.test_bnd (Trapdiag.get_trap td x).Trapezoid.left
		   (fun l -> l) 0.) in
      List.sort (fun x y -> int_of_float (2. *. ((bnd x) -. (bnd y))) ) l

  (* add object with index oix in set s and conflict graph cg to region set
     f *)
  let add_obj obj oix cg s f =
    let regions = Conflictgraph.regions_of cg oix in
    let split rix =		(* split region with given object *)
      printf "processing region: %d\n" rix;
      let trap = Trapdiag.get_trap f rix in
      let conflicting_objs = Conflictgraph.objects_of cg rix in
	begin
	  (* remove conflict of new object with this region *)
	  Conflictgraph.remove cg oix rix;
	  let children = Trapezoid.split trap obj in (* split trap *)
	  let childrenix = Trapdiag.add_children f children
	  and objs_to_split = List.filter ((<>) oix) conflicting_objs
	  and intersecting_children = ref [] in
	    printf "childrenix: ";
	    List.iter (printf "%d ") childrenix; printf "\n";
	    Printf.printf "%d objects to split \n" (List.length objs_to_split);
	    List.iter
	      (fun ix ->
		 (* remove conflict with split region *)
		 Conflictgraph.remove cg ix rix;
		 printf "split obj %d intersecting region %d:" ix rix;
		 let intx_children =
		   List.filter (fun cix -> intersect s.(ix)
				  (Trapdiag.get_trap f cix)) childrenix in
		   (* update conflict graph *)
		   List.iter (printf "%d ") intx_children; printf "\n";
		   List.iter (fun cix -> Conflictgraph.add cg ix cix)
		     intx_children
	      ) objs_to_split;
	    flush_all ();
	    Graphics.clear_graph ();
	    Graphics.set_color Graphics.black;
	    Trapdiag.draw f;
	    Graphics.set_color Graphics.blue;
	    List.iter Trapezoid.draw (Trapdiag.children_list children);
	    wait_key();
	    childrenix					       
	end
    and merge_traps unsorted_traps =
      let td = f in
      let rec iter_pairs f l = match l with
	  a::b::tail ->
	    let r = f a b in
	      ( match r with
		    Some t -> iter_pairs f (tail)
		  | None -> iter_pairs f (b::tail) )
	| [a] -> ()
	| [] -> () in
      let s_traps = sort_traps td unsorted_traps in
      let (upper,lower) = List.partition
			    (fun ix -> let t = Trapdiag.get_trap td ix in
			       t.Trapezoid.lower=Trapezoid.Closed obj) s_traps
      and process tnix tnpix =
	let tn = Trapdiag.get_trap td tnix
	and tnp = Trapdiag.get_trap td tnpix in
	  Graphics.set_color Graphics.blue;
	  Trapezoid.draw tn;
	  Graphics.set_color Graphics.red;
	  Trapezoid.draw tnp;
	  printf "processing traps %d %d\n" tnix tnpix;
	  printf "trap %d's left: " tnix;
	  Lineseg.print (Trapezoid.left_seg tn); printf "\n";
	  wait_key ();
	  Graphics.set_color Graphics.black;
	  Trapezoid.draw tn; Trapezoid.draw tnp;
	  if (tn.Trapezoid.upper)=(tnp.Trapezoid.upper) 
	    && (tn.Trapezoid.lower)=(tnp.Trapezoid.lower) then
	      let t = { Trapezoid.left = tn.Trapezoid.left;
			Trapezoid.upper = tn.Trapezoid.upper;
			Trapezoid.right = tnp.Trapezoid.right;
			Trapezoid.lower = tn.Trapezoid.lower;
			Trapezoid.tl = tn.Trapezoid.tl;
			Trapezoid.tr = tnp.Trapezoid.tr;
			Trapezoid.br = tnp.Trapezoid.br;
			Trapezoid.bl = tn.Trapezoid.bl } in
	      let r1 = Trapdiag.remove_trap td tnix
	      and r2 = Trapdiag.remove_trap td tnpix
	      and tix = Trapdiag.add_trap t td in
		printf "MERGING traps %d %d\n" tnix tnpix;
		let objs1 = Conflictgraph.objects_of cg tnix
		and objs2 = Conflictgraph.objects_of cg tnpix in
		  List.iter (fun x -> Conflictgraph.remove cg x tnix) objs1;
		  List.iter (fun x -> Conflictgraph.remove cg x tnpix) objs2;
		  List.iter (fun x -> Conflictgraph.add cg x tix )
		    (objs1 @ objs2);
		  Some tix
	  else
	    None
      in
	printf "*** Merging traps: ";
	flush_all ();
	Graphics.clear_graph ();
	Graphics.set_color Graphics.black;
	Trapdiag.draw f;	  
	iter_pairs process upper;
	iter_pairs process lower
    in
      printf "conflicting regions: ";
      List.iter (fun x -> printf "%d:" x ) regions;
      printf "\n";
      (* split regions with given object *)
      let children = List.concat (List.map split regions)
      and traps = List.map (Trapdiag.remove_trap f) regions in
	(* and finally merge traps *)
	merge_traps children

  let print_obj ls = Lineseg.print ls
end


module ConsTrapDiag = Randomizedincr.Make (LineSegIntxn)
	  
