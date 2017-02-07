(*
**
** ocaml module Lineseg
**
** Description: Geometric primitives for points and line segments
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

type point = float * float

let print_pnt (x,y) = Printf.printf "(%f,%f)" x y

let x p = fst p
let y p = snd p

let add (x1,y1) (x2,y2) = (x1+.x2,y1+.y2)
let sub (x1,y1) (x2,y2) = (x1-.x2,y1-.y2)
let mul k (x1,y1) = (k*.x1,k*.y1)

let length (x0,y0) = sqrt (x0*.x0 +. y0*.y0)

type lineseg = point * point

let leftx ls (x1,y1) (x2,y2) = min x1 x2
let rightx ls (x1,y1) (x2,y2) = max x1 x2

let center (p1,p2) = mul 0.5 (add p1 p2)

type pnt_classification = Left | Right | Beyond | Behind | Between |
  Origin | Destination

(* I love those floating point errors :( *)
let epsilon = 0.0000000001

(* classify point p2 wrt a linesegment *)
let classify_point p2 (p0,p1) = 
    	let m=sub p1 p0 and n = sub p2 p0 in 
 	let area=x m *. y n -. x n *. y m in
 	  if area>epsilon then Left
	  else if area<(-.epsilon) then Right
	  else if x m*.x n < 0.0 || y m *. y n < 0.0 then Behind
	  else if length m < length n then Beyond
	  else if p0=p2 then Origin
	  else if p1=p2 then Destination
  	  else Between
 
let left_inclusive p ls =
  let cls = classify_point p ls in
    cls=Left or cls=Origin or cls=Destination or cls=Between
    
let right_inclusive p ls =
  let cls = classify_point p ls in
    cls=Right or cls=Origin or cls=Destination or cls=Between
	
let string_of_classification c = match c with
     Left -> "left"
  | Right -> "right"
  | Beyond -> "beyond"
  | Behind -> "behind"
  | Between -> "between"
  | Origin -> "origin"
  | Destination -> "destination"

let make p1 p2 = if x p1 <= x p2 then (p1,p2) else (p2,p1)
let print (p1,p2) = print_pnt p1; Printf.printf "--"; print_pnt p2
let plot (x0,y0) = Graphics.draw_circle (int_of_float x0) (int_of_float y0) 3
let plot_big (x0,y0) = Graphics.draw_circle (int_of_float x0) (int_of_float y0) 5

let draw ( (x0,y0), (x1,y1) ) =
  Graphics.moveto (int_of_float x0) (int_of_float y0); 
  Graphics.lineto (int_of_float x1) (int_of_float y1)

let left p = fst p
let right p = snd p

(* line eqn y = ax + b *)

let slope ( (x1,y1), (x2,y2)  ) = (y2-.y1) /. (x2-.x1)

let horiz_intxn ( (p0,p1): lineseg) yi = 
  let alpha = (yi-.y p1) /. (y p0 -.y p1) in
  let xi = x p1 +. alpha *. (x p0 -. x p1) in
    if 1.0 >= alpha & alpha >= 0.0 then Some (xi,yi) else None

let vert_intxn ( (p0,p1): lineseg) xi = 
  let alpha = (xi-.x p1) /. (x p0 -.x p1) in
  let yi = y p1 +. alpha *. (y p0 -. y p1) in
    if 1.0 >= alpha & alpha >= 0.0 then Some (xi,yi) else None

let dot u v = x u *. x v +. y u *. y v  (* dot product *)
let perp (u1,u2) (v1,v2) = u1*.v2 -. u2*.v1 (* perpendicular product *)
			     
(* intersection of linesegments (p00,p01) and (p10,p11)
 * u = p01 - p 00 and v = p11 - p10
 * P = p00 + s u, Q = p11 + t v
 *)
let intxn (((x1,y1) as p00), ((x2,y2) as p01))
  (((x3,y3) as p10), ((x4,y4) as p11)) =
  let u = sub p01 p00 and v = sub p11 p10 and w = sub p00 p10 in
  let d = perp u v in
    if abs_float d < epsilon then 
      if perp u w <> 0. || perp v w <> 0. then
	None
      else (* collinear, do they overlap? *)
	let w2 = sub p01 p10 in
	let (ta,tb) = if x v <> 0. then
	  (x w /. x v, x w2 /. x v) else (y w /. y v, y w2 /. y v) in
	let (t0,t1) = (min ta tb, max ta tb) in
	  if (t0 > 1. || t1 < 0.) then
	    None
	  else
	    Some (add p00 (mul t0 u)) (* behave as if they intersect
					 in a single point -- wrong *)
    else
      let si = perp v w /. d
      and ti = perp u w /. d in
	if (si < 0. || si > 1.) || (ti < 0. || ti > 1.) then
	  None
	else
	  Some (add p00 (mul si u))


	
	
      
    
					  
