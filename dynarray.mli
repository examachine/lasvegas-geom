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

val make : 'a -> 'a dynarray

val length : 'a dynarray -> int

val get : 'a dynarray -> int -> 'a

val set : 'a dynarray -> int -> 'a -> unit

val vec : 'a dynarray -> 'a array

val mapa : ('a -> 'b) -> 'a dynarray -> 'b array

val mapai : (int -> 'a -> 'b) -> 'a dynarray -> 'b array

val iteri : (int -> 'a -> unit) -> 'a dynarray -> unit
