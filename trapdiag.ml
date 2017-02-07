(*
**
** ocaml module Trapdiag
**
** Description: A trapezoidal map (vertical visibility map) that is quite
** limited in functionality and suitable only for the algorithm used 
** in this project
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Printf
open Lineseg
open Trapezoid

type trapdiag = {
  array : trapezoid Dynarray.dynarray;
  mutable dead : int list;
  adj: Graph.graph;
}
 
let empty_trapdiag () = {
  array = Dynarray.make empty_trap;
  dead = [];
  adj = Graph.make ();
}

let get_trap diag ix = Dynarray.get diag.array ix

let add_trap trap diag = match diag.dead with
    [] -> let ix = Dynarray.length diag.array in
      Dynarray.set diag.array ix trap; ix
  | head :: tail ->
      diag.dead <- tail;
      Dynarray.set diag.array head trap; head

(* remove a trap and return it *)
let remove_trap diag ix =
  let trap = Dynarray.get diag.array ix in
    Dynarray.set diag.array ix empty_trap;
    diag.dead <- ix :: diag.dead;
    trap
      
(*let adjacent_traps diag ix =
  []

let intersect_trap diag ix ls  =
  true
*)

let draw diag =
  let draw ix x =
    if not (List.exists ((=) ix) diag.dead) then Trapezoid.draw x else () in
  let mark_center ix t =
    if not (List.exists ((=) ix) diag.dead) then
      let (x,y) =  Trapezoid.center t in
	Graphics.moveto (int_of_float x) (int_of_float y);
	Graphics.draw_string (string_of_int ix)
    else ()
  in
    Array.iteri draw (Dynarray.vec diag.array);
    Array.iteri mark_center (Dynarray.vec diag.array)

let add_children diag (cl,ct,cr,cb) =
  [ add_trap ct diag; add_trap cb diag ]
  @ ( if cl<>Trapezoid.empty_trap then
        [add_trap cl diag] else [] )
  @ ( if cr<>Trapezoid.empty_trap then
	[add_trap cr diag] else [] )

let children_list (cl,ct,cr,cb) =
  let check x =
    if x <>Trapezoid.empty_trap then
      [ x  ]
    else
      [] in
    List.concat (List.map check [cl;ct;cr;cb])

(* report_traps *)

