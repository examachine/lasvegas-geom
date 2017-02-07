(*
**
** ocaml module Randomizedincr
**
** Description: Generic randomized incremental construction algorithm
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Printf

module type GeomIntxn =
sig
  type obj

  type region
  type region_set (*indexed by integers*)

  val init_regionset : unit -> region_set
  val add_obj : obj -> int -> Conflictgraph.t -> obj array -> region_set -> unit
      
  val print_obj :  obj -> unit
end

module Make = functor (Geom: GeomIntxn) ->
struct
  let random_permute n =
    let a = Array.init n (function i -> i)
    and swap a i j = let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
    in
      Random.self_init ();
      for i=1 to n-1 do
	swap a (Random.int i) i
      done;
      a
  let construct s =
    let n = Array.length s
    in
    let p = random_permute n
    and g = Conflictgraph.make s
    and f = Geom.init_regionset ()
    in
      Array.iter (fun x->printf "%d " x) p;
      for i=0 to n-1 do
	let j = p.(i) in
	  Printf.printf "adding object %d :" j;
	  Geom.print_obj s.(j); Printf.printf "\n";
	  Geom.add_obj s.(j) j g s f;
      done;
      f
end
