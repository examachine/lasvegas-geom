(*
**
** ocaml module Trapezoid
**
** Description: Geometric primitives for trapezoids
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Lineseg
open Printf

type 'a bound = Open
		| Closed of 'a

type horizbound = lineseg bound
type verticalbound = float bound

let cons_hcbnd b obj =  match b with
    Open -> Open
  | Closed x -> Closed (obj)


type trapezoid = {
  mutable upper : horizbound;
  mutable lower : horizbound;
  mutable left: verticalbound;
  mutable right: verticalbound;
  mutable tl: point;
  mutable tr: point;
  mutable bl: point;
  mutable br: point
}

let print t =
  printf "tl (%f,%f), tr (%f,%f), bl (%f,%f), br (%f,%f)\n"
    (x t.tl) (y t.tl)
    (x t.tr) (y t.tr)
    (x t.bl) (y t.bl)
    (x t.br) (y t.br)


let draw_faces (list: lineseg list) =
  match list with
    [] -> ()
  | _ ->
      let (a,b) = Graphics.current_point () in 
      let ls0 = List.hd list in
	List.iter (fun x -> Lineseg.draw x) list;
	Graphics.moveto a b

let empty_trap = { upper = Open; lower = Open;
		   left = Open; right = Open;
		   tl = (0.,500.); tr = (500.,500.);
		   bl = (0.,0.); br = (500.,0.) }

let upper_seg trap = (trap.tl, trap.tr)
let right_seg trap = (trap.tr, trap.br)
let lower_seg trap = (trap.br, trap.bl)
let left_seg trap = (trap.bl, trap.tl)

let faces trap =
  (match trap.upper with
      Open -> []
     | Closed _ -> [(trap.tl, trap.tr)]: lineseg list)
  @ (match trap.right with
	 Open -> []
       | Closed _ -> [(trap.tr, trap.br)])
  @ (match trap.lower with
	 Open -> []
       | Closed _ -> [(trap.br, trap.bl)])
  @ (match trap.left with
	 Open -> []
       | Closed _ -> [(trap.bl, trap.tl)])
    
let draw trap = draw_faces (faces trap)

(* find center *)
let center trap = 
  let c = List.map Lineseg.center (faces trap) in
  let n = List.length c in
    Lineseg.mul (1.0 /. (float_of_int n) )
      (List.fold_left Lineseg.add (0.0,0.0) c)

(* lame function, yeah i know *)
let force_opt opt = match opt with
    None -> raise Not_found
  | Some x -> x 

(* is a given point inside the trapezoid? *)
let inside_bnd (b, ls) pnt = match b with
    Open -> true
  | Closed _ -> match intxn ls pnt with
	None -> false
      |	Some x -> true

let swap x y =
  let t = !x in x := !y; y := t

let do_maybe a f = match a with
    None -> ()
  | Some i -> f i

let maybe a f x = match a with
    None -> x
  | Some i -> f i

let test_bnd a f t = match a with
    Open -> t
  | Closed i -> f i

let inside trap pnt =
  let a = test_bnd (trap.left) (fun l -> x pnt >= l) true
  and b = test_bnd (trap.right) (fun r -> x pnt <= r) true
  and c = test_bnd (trap.upper) (fun u -> right_inclusive pnt u) true
  and d = test_bnd (trap.lower) (fun l -> left_inclusive pnt l) true
  in
    printf "left %b, right %b, upper %b, lower %b\n"  a b c d;
    a & b & c & d

(* a list of points at which a line segment intersects a trapezoid *)
let intersect_bounds trap ls =
  List.concat (List.map
		 (fun x -> (match intxn x ls with None -> [] | Some x -> [x]))
		 (faces trap))

let intersect trap (p0,p1) =
(*  printf "check segment "; print (p0,p1);
  printf "p0 inside? %b, p1 inside %b?\n" (inside trap p0) (inside trap p1);
  printf "intersecting points ";
  List.iter print_pnt (intersect_bounds trap (p0,p1)); printf "\n"; *)
  (inside trap p0 && inside trap p1)
  or (List.length (intersect_bounds trap (p0,p1)) > 0)

(* split a trapezoid by a line segment into potentially four
   child trapezoids cl: left, ct: top, cb: bottom, cr: right *)

let split trap ((p0,p1) as ls) =
  let til = ref trap.tl (* top intersection left *)
  and tir = ref trap.tr (* top intersection right *)
  and bil = ref trap.bl (* bottom intersection left *)
  and bir = ref trap.br (* bottom intersection right *)
  and sl = ref p0 (* split right point, same as vertical split points... *)
  and sr = ref p1 (* split left point *) in
    if x !sl > x !sr then swap sl sr else (); (* sl to left of sr *)
    let p0 = !sl and p1 = !sr and ls = (p0,p1) in (* correct arguments *)


      (* lower/upper intersection tests *)
      do_maybe (intxn ls (upper_seg trap))
	(fun i ->
	   printf "*** intersection"; print_pnt i; printf "\n";
	   if classify_point !sl (upper_seg trap) = Left then
	     (sl:=i;til:=i)
	   else
	     (sr:=i;tir:=i));
      do_maybe (intxn ls (lower_seg trap))
	(fun i ->
	   printf "*** intersection"; print_pnt i; printf "\n";
	   if classify_point !sl (lower_seg trap) = Left then
	     (sl:=i;bil:=i)
	   else
	     (sr:=i;bir:=i));
      
      let c = classify_point !sl (upper_seg trap) in
	printf "**** %s\n" (string_of_classification c);

      (* lower/upper visibility tests *)
      if (inside trap !sl) then
	begin
	  (*Graphics.set_color Graphics.red; plot_big !sl;*)
	  do_maybe (vert_intxn (upper_seg trap) (x !sl)) (fun i -> til:=i);
	  do_maybe (vert_intxn (lower_seg trap) (x !sl)) (fun i -> bil:=i);
	end else ();
      if (inside trap !sr) then
	begin
	  (*Graphics.set_color Graphics.black; plot_big !sr;*)
	  do_maybe (vert_intxn (upper_seg trap) (x !sr)) (fun i -> tir:=i);
	  do_maybe (vert_intxn (lower_seg trap) (x !sr)) (fun i -> bir:=i);
	end else ();

      (* vertical bound intersections *)
      do_maybe (intxn ls (left_seg trap)) (fun i -> sl:=i);
      do_maybe (intxn ls (right_seg trap)) (fun i -> sr:=i);
      Graphics.set_color Graphics.green;
      draw trap;
      (*Graphics.set_color Graphics.red;
      plot !til; plot !tir;
      plot !bil; plot !bir;
      Graphics.set_color Graphics.magenta;
      plot !sl; plot !sr;
      Graphics.set_color Graphics.green;
      let g=Graphics.wait_next_event [Graphics.Button_down] in ();*)

      (* construct child traps *)
      let cl = if !til <> trap.tl then
	{ left = trap.left;
	  upper = trap.upper;
	  right = Closed (x !til);
	  lower = trap.lower;
	  tl = trap.tl;
	  tr = !til;
	  br = !bil;
	  bl = trap.bl } else empty_trap
      and cr = if !tir <> trap.tr then
	{ left = Closed (x !tir);
	  upper = trap.upper;
	  right = trap.right;
	  lower = trap.lower;
	  tl = !tir;
	  tr = trap.tr;
	  br = trap.br;
	  bl = !bir } else empty_trap
      and ct = 
	{ left = Closed (x !til);
	  upper = trap.upper;
	  right = Closed (x !tir);
	  lower = Closed ls;
	  tl = !til;
	  tr = !tir;
	  br = !sr;
	  bl = !sl }
      and cb = 
	{ left = Closed (x !til);
	  upper = Closed ls;
	  right = Closed (x !tir);
	  lower = trap.lower;
	  tl = !sl;
	  tr = !sr;
	  br = !bir;
	  bl = !bil }
      in
	(cl, ct, cr, cb) (* return child traps *)

	

	     
