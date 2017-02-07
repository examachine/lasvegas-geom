(*
**
** ocaml module Dynarray
**
** Description: Array with dynamic size
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

type 'a dynarray = { mutable vec: 'a array; mutable size: int;
		     f: (int -> 'a) }

let make x = { vec = Array.make 1 x;
	       size = 0;
	       f = function _ -> x }

let length a = a.size

let adjust_size a ix =
  if ix+1 > a.size then a.size <- ix+1 else ();
  if ix >= Array.length a.vec then
    let new_size = max (ix+1) (Array.length a.vec * 2) in
    let new_vec = Array.init new_size a.f in
      begin
	Array.blit a.vec 0 new_vec 0 (Array.length a.vec);
	a.vec <- new_vec;
      end
  else
    ()

let get a ix =
  adjust_size a ix;
  a.vec.(ix)

let set a ix v =
  adjust_size a ix;
  a.vec.(ix) <- v

let vec a = Array.sub a.vec 0 a.size

let mapa f a = Array.map f (vec a)

let mapai f a = Array.mapi f (vec a)

let iteri f a = Array.iteri f (vec a)


